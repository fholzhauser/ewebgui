-module(ewg_apps).
-export([store_app/2, list_apps/0, delete_app/1, request/2, make_menu_structure/0]).
-import(ewg_lib, [plget/2, plset/3]).

%% some silly primitive format check
store_app(Path, {AppTag, Name, Description, Info}) ->
    %% Info might be composed by multiple proplists i.e. developer might
    %% delegate parts of definitions to the modules handling them. Merge these.
    MergedInfo = lists:foldl(
        fun
            ({Key, Value}, Acc) -> [{Key, Value} | Acc];
            (PropList, Acc) when is_list(PropList) -> merge_keys(PropList, Acc)
        end,
        [],
        Info
    ),
    ewg_conf:write(
        apps,
        ewg_lib:plset(
            Path,
            {AppTag, Name, Description, MergedInfo},
            ewg_conf:read(apps, [])
        )
    );
store_app(_, _) ->
    {error, wrong_info_format}.

list_apps() ->
    ewg_conf:read(apps, []).

delete_app(Path) ->
    ewg_conf:write(
        apps,
        ewg_lib:pldel(
            Path,
            ewg_conf:read(apps, [])
        )
    ).

request([], _) ->
    %% root path should take home
    ewg_templates:home();

request(ReqPath, Params) ->
    %% reverse sort it to make sure it finds the longest prefix first
    case find_prefixmatch(ReqPath, list_apps()) of
        {ok, RestPath, {AppTag, Name, _Description, Info}} ->
            put(ewg_current_app, AppTag),
            put(ewg_current_appname, Name),
            put(ewg_current_path, ReqPath),
            case plget(handlers, Info) of
                undefined ->
                    {error, app_handler_not_found};
                Handlers ->
                    UserPermissions = proplists:get_value(AppTag, ewg_access:get_permissions()),
                    Result = find_handler(RestPath, Params, Handlers, UserPermissions),
                    case ewg_conf:read(enable_default_user_log, false) of
                        true ->
                            ewg_user_log:log();
                        _ ->
                            ok
                    end,
                    Result
            end;
        {error, not_found} ->
            {error, app_callback_not_found}
    end.

%% this will be a recursive check as we dive in the handler tree
find_handler(ReqPath, Params, Handlers, UserPermissions) ->
    case find_prefixmatch(ReqPath, Handlers) of
        {ok, RestPath, {Access, _, Action}} ->
            %% the proplist API is handy here that it returns true if key is
            %% present with no value
            case {proplists:get_value(Access, UserPermissions), Action} of
                {undefined, _} ->
                    {error, no_access};
                {true, {handlers, _}} ->
                    %% No more permission but not end node
                    {error, no_access};
                {RestPermissions, {handlers, H}} ->
                    find_handler(RestPath, Params, H, RestPermissions);
                {true, {route, R}} ->
                    match_route(Params, R, []);
                {RestPermissions, {route, R}} ->
                    match_route(Params, R, RestPermissions);
                {true, {Validation, Callback}} ->
                    validate_and_call(Params, Validation, Callback, []);
                {RestPermissions, {Validation, Callback}} ->
                    validate_and_call(Params, Validation, Callback, RestPermissions)
            end;
        _Err ->
            {error, app_handler_not_found}
    end.

match_route(Params, [{Match, {Validation, Callback}}| Rest], Permissions) ->
    case [MatchItem || MatchItem  <- Match, not match_param(MatchItem, Params)] of
        [] ->
            validate_and_call(Params, Validation, Callback, Permissions);
        _ ->
            match_route(Params, Rest, Permissions)
    end;
match_route(_, [], _RestPermissions) ->
    {error, no_route_match}.

match_param({'not', Param}, List) -> not match_param(Param, List);
match_param({Name, Value}, List) -> lists:member({Name, Value}, List);
match_param(Name, List) when is_list(List) -> lists:keymember(Name, 1, List).

validate_and_call(Params, Validations, {M, F}, Permissions) ->
    validate_and_call(Params, Validations, {M, F, []}, Permissions);
validate_and_call(Params, Validations, {M, F, A}, Permissions) ->
    %% collapse
    %% validation can be used eventually standalone too, therefore in ewg_lib
    {Errors, Validated} = ewg_lib:validate(Params, Validations),
    put(ewg_validations_results, Errors),
    put(ewg_orig_form_params, Params),
    put(ewg_form_params, case Errors of [] -> Validated; _ -> Params end),
    put(ewg_current_call, {M, F, A}),
    %% All parameters are on the process dictionary there is no need to
    %% give it to the function.
    %% Errors we only give indicationto make it easy matching ok and error
    %% handlers: validated or validation_failed
    apply(M, F, [case Errors of [] -> validated; _ -> validation_failed end, Permissions | A]).

make_menu_structure() ->
    Permissions = ewg_access:get_permissions(),
    lists:foldl(
        fun(App, Acc) ->
            case get_app_menus(App, Permissions) of
                [] -> Acc;
                Menu -> Acc ++ [Menu]
            end
        end,
        [{"Home", "/", []}],
        list_apps()
    ).

get_app_menus({AppPath, {AppTag, Name, _Description, Info}}, Permissions) ->
    case proplists:get_value(AppTag, Permissions) of
        undefined ->
            [];
        true ->
            %% application level menu without futher permission levels ...
            [];
        RestPermissions ->
            case ewg_lib:plget(handlers, Info) of
                undefined -> [];
                Handlers ->
                    case app_menu(Handlers, RestPermissions, AppPath) of
                        [] -> [];
                        Menus -> {Name, AppPath, Menus}
                    end
            end
    end;
get_app_menus(_, _) -> [].

app_menu(Handlers, Permissions, MainPath) ->
    app_menu(Handlers, Permissions, [], MainPath).

app_menu([{_, {_, nomenu, _}}| HRest], Permissions, Acc, PathAcc) ->
    app_menu(HRest, Permissions, Acc, PathAcc);
app_menu([{Path, {Access, MenuName, Action}}| HRest], Permissions, Acc, PathAcc) ->
    case {proplists:get_value(Access, Permissions), Action} of
        {undefined, _} ->
            app_menu(HRest, Permissions, Acc, PathAcc);
        {true, {handlers, _}} ->
            %% No deeper permissions but not the end of the structure, skip this
            app_menu(HRest, Permissions, Acc, PathAcc);
        {RestPermissions, {handlers, H}} ->
            %% More permissions, more handlers, good
            case app_menu(H, RestPermissions, [], PathAcc ++ Path) of
                [] ->
                    app_menu(HRest, Permissions, Acc, PathAcc);
                SubMenus ->
                    app_menu(HRest, Permissions, Acc ++ [{MenuName, PathAcc ++ Path, SubMenus}], PathAcc)
            end;
        {_, _} ->
            %% Permission on current level and end of handler tree (route or direct callback)
            app_menu(HRest, Permissions, Acc ++ [{MenuName, PathAcc ++ Path, []}], PathAcc)
    end;
app_menu([], _, Acc, _) -> Acc.


find_prefixmatch(String, List) ->
    %% reverse sort to make sure it finds the longest prefix first
    find_prefixmatch1(String, lists:reverse(lists:sort(List))).

find_prefixmatch1(String, [{Prefix, Result}| Rest]) ->
    case lists:prefix(Prefix, String) of
        true ->
            {ok, lists:nthtail(length(Prefix), String), Result};
        false ->
            find_prefixmatch1(String, Rest)
    end;
find_prefixmatch1(_, []) ->
    {error, not_found}.

merge_keys([{Key, Val} | Rest], Props) ->
    case plget(Key, Props) of
        undefined ->
            %% Merge by appending to keep the order ...
            merge_keys(Rest, Props ++ [{Key, Val}]);
        Value ->
            merge_keys(Rest, plset(Key, Value ++ Val, Props))
    end;
merge_keys([], Props) -> Props.
