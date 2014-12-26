-module(ewg_access).
-compile(export_all).
-import(ewg_lib, [plget/2, plget/3, plset/3, pldel/2, id/0]).
-include_lib("ewebgui/include/ewebgui.hrl").

%%===========================================
%% Form management functions
%%===========================================
%% revert form params to before validation, should anything fail after validation
%% and the original values should be rendered
revert_form_params() ->
    put(ewg_form_params, get(ewg_orig_form_params)).

%% get the form parameters for use in e.g. input macros to fill values
get_form_params() ->
    case get(ewg_form_params) of undefined -> []; Params -> Params end.

get_form_param(Name) ->
    plget(Name, get_form_params()).

get_form_param(Name, Default) ->
    plget(Name, get_form_params(), Default).

%% set form parameter so they can be filled by the input macros when rendering
set_form_params(Params) ->
    put(ewg_form_params, Params).

set_form_param(Name, Value) ->
    put(ewg_form_params, plset(Name, Value, get_form_params())).

%% get_validation results for rendering forms with validation errors
get_validation_results() ->
    case get(ewg_validations_results) of undefined -> []; Results -> Results end.

get_validation_result(Name) ->
    plget(Name, get_validation_results(), []).

%% set validation result allows external validation functions to push their results to the current result set
set_validation_result(Name, Text) ->
    put(ewg_validations_results, plset(Name, Text, get_validation_results())).

%% here we append the new result to the already existing list of that parameter
add_validation_result(Name, Text) ->
    put(ewg_validations_results, plset(Name, get_validation_result(Name) ++ [Text], get_validation_results())).

%% here we can push a complete new set of validation errors : should be [{Name, Text}, .... ]
set_validation_results(Results) ->
    put(ewg_validations_results, Results).

clear_validation_result(Name) ->
    put(ewg_validations_results, pldel(Name, get_validation_results())).

%% silly name for both updating value in case of success or result in case of error
push_validation(Name, {ok, Value}) ->
    set_form_param(Name, Value);
push_validation(Name, {error, Text}) ->
    add_validation_result(Name, Text).

%% set form fields to disable
disable_form_params() ->
    disable_form_params(all).

disable_form_params(Params) ->
    put(ewg_disabled_form_params, Params).

%% check it
is_form_param_disabled(Name) ->
    case get(ewg_disabled_form_params) of
        all -> true;
        Params when is_list(Params) -> lists:member(Name, Params);
        _ -> false
    end.

%%===========================================
%% User management functions
%%===========================================

block_user(User, Minutes) ->
    block_user(User, Minutes, []).

block_user(User, Minutes, EventInfo) ->
    Until = ewg_lib:now_add(Minutes * 60),
    set_user_status(User, {blocked, Until}, EventInfo).

suspend_user(User) ->
    set_user_status(User, suspended, []).

suspend_user(User, EventInfo) ->
    set_user_status(User, suspended, EventInfo).

reset_user(User) ->
    set_user_status(User, active, []).

reset_user(User, EventInfo) ->
    set_user_status(User, active, EventInfo).

get_user_status(User) ->
    case get_user_var(User, ewg_user_status) of
        undefined -> set_user_var(User, active), active;
        Status -> Status
    end.

set_user_status(User, NewStatus, EventInfo) ->
    CurrentStatus = get_user_status(User),
    ?EVENT(ewg_user_status_changed, [{old_status, CurrentStatus}, {new_status, NewStatus}, {user, User} | EventInfo]),
    set_user_var(User, ewg_user_status, NewStatus).

increase_login_count(User) ->
     case get_user_var(User, ewg_login_count) of
        undefined -> set_user_var(User, ewg_login_count, 1), 1;
        Cnt -> set_user_var(User, ewg_login_count, Cnt + 1), Cnt
    end.

reset_login_count(User) ->
    set_user_var(User, ewg_login_count, 0).

set_request_user(User) ->
    put(ewg_user, User).

check_user_login(UserName, Password) ->
    EventDefaults = [{login_name_sent, UserName}],
    case get_user_data(UserName) of
        undefined ->
            ?EVENT(ewg_login_fail, [{reason, wrong_user}| EventDefaults]),
            {error, wrong_user};
        UserData ->
            case plget(password, UserData) == erlang:phash2(Password) of
                true ->
                    case get_user_status(UserName) of
                        suspended ->
                            ?EVENT(ewg_login_fail, [{reason, suspended} | EventDefaults]),
                            {error, user_suspended};
                        {blocked, Ts} ->
                            Now = os:timestamp(),
                            if
                                Now > Ts ->
                                    set_request_user(UserName),
                                    ?EVENT(ewg_login_success, [{reason, blocking_expired} | EventDefaults]),
                                    reset_user(UserName),
                                    ok;
                                true ->
                                    ?EVENT(ewg_login_fail, [{reason, blocked} | EventDefaults]),
                                    {error, blocked}
                            end;
                        active ->
                            set_request_user(UserName),
                            ?EVENT(ewg_login_success, EventDefaults),
                            ok
                    end;
                _ ->
                    Count = increase_login_count(UserName),
                    MaxAttempts = ewg_conf:read(ewg_login_max_attempts, 3),
                    if
                        Count > MaxAttempts ->
                            ?EVENT(ewg_login_fail, [{reason, wrong_password}, {login_attempt_count, Count+1} | EventDefaults]),
                            case ewg_conf:read(ewg_login_max_attempt_action, {block, 10}) of
                                suspend ->
                                    reset_login_count(UserName),
                                    suspend_user(UserName);
                                {block, Mins} ->
                                    reset_login_count(UserName),
                                    block_user(UserName, Mins)
                            end,
                            {error, too_many_attempts};
                        true ->
                            ?EVENT(ewg_login_fail, [{reason, wrong_password}, {login_attempt_count, Count+1} | EventDefaults]),
                            {error, wrong_password}
                    end
            end
    end.

list_users() ->
    mnesia:dirty_all_keys(ewg_user).

add_user(UserName, Password, Data) ->
    case mnesia:dirty_read(ewg_user, UserName) of
        [] ->
            mnesia:dirty_write(
                {ewg_user, UserName,
                    plset(password, erlang:phash2(Password), Data)
                }
            ),
            ok;
        [_User] ->
            {error, user_exists}
    end.

set_user_var(Key, Val) ->
    set_user_var(get_user(), Key, Val).

set_user_var(UserName, Key, Val) ->
    case mnesia:dirty_read(ewg_user, UserName) of
        [] ->
            {error, no_user};
        [{_, UserName, Data}] ->
            mnesia:dirty_write(
                {ewg_user, UserName,
                    ewg_lib:plset(Key,
                        if
                            Key == password -> erlang:phash2(Val);
                            true -> Val
                        end,
                    Data)
                }
            ),
            ok
    end.

delete_user_var(Key) ->
    delete_user_var(get_user(), Key).

delete_user_var(UserName, Key) ->
    case mnesia:dirty_read(ewg_user, UserName) of
        [] ->
            {error, no_user};
        [{_, UserName, Data}] ->
            mnesia:dirty_write({ewg_user, UserName, ewg_lib:pldel(Key,Data)}),
            ok
    end.


get_permissions() ->
    get_group_var(permissions).

%% get particular permission e.g. get_permission([ewg_admin, users, add])
%% returns undefined | true | Perm
get_permission(Permission) ->
    ewg_lib:dplget(Permission, get_permissions()).

get_user() ->
    get(ewg_user).

get_user_var(Key) ->
    get_user_var(get_user(), Key).

get_user_var(UserName, Key) ->
    case mnesia:dirty_read(ewg_user, UserName) of
        [{_, UserName, Data}] -> plget(Key, Data);
        _ -> undefined
    end.

get_user_data() ->
    get_user_data(get_user()).

get_user_data(UserName) ->
    case mnesia:dirty_read(ewg_user, UserName) of
        [{_, UserName, Data}] -> Data;
        _ -> undefined
    end.

delete_user(UserName) ->
    delete_user_sessions(UserName),
    mnesia:dirty_delete(ewg_user, UserName).

list_user_sessions(User) ->
    [{Cookie, Start, ClientIp} || {Cookie, Start, SUser, ClientIp} <- list_all_sessions(), SUser == User].

delete_user_sessions(User) ->
    [ewebgui_appmod:delete_session(Cookie) || {Cookie, _, _} <- list_user_sessions(User)],
    ok.

list_all_sessions() ->
    ewebgui_appmod:list_sessions().

list_groups() ->
    mnesia:dirty_all_keys(ewg_group).

add_group(GroupName, Data) ->
    case mnesia:dirty_read(ewg_group, GroupName) of
        [] ->
            mnesia:dirty_write({ewg_group, GroupName, Data}),
            ok;
        [_Group] ->
            {error, group_exists}
    end.

set_group_var(GroupName, Key, Val) ->
    case mnesia:dirty_read(ewg_group, GroupName) of
        [] ->
            {error, no_group};
        [{_, GroupName, Data}] ->
            mnesia:dirty_write(
                {ewg_group, GroupName, ewg_lib:plset(Key, Val, Data)}
            ),
            ok
    end.

delete_group_var(GroupName, Key) ->
    case mnesia:dirty_read(ewg_group, GroupName) of
        [] ->
            {error, no_group};
        [{_, GroupName, Data}] ->
            mnesia:dirty_write({ewg_group, GroupName, ewg_lib:pldel(Key, Data)}),
            ok
    end.


get_group_data() ->
    get_group_data(get_user_var(group)).

get_group_data(GroupName) ->
    case mnesia:dirty_read(ewg_group, GroupName) of
        [{_, GroupName, Data}] -> Data;
        _ -> undefined
    end.

get_group_var(Key) ->
    get_group_var(get_user_var(group), Key).

get_group_var(GroupName, Key) ->
    case mnesia:dirty_read(ewg_group, GroupName) of
        [{_, GroupName, Data}] -> plget(Key, Data);
        _ -> undefined
    end.

delete_group(GroupName) ->
    mnesia:dirty_delete(ewg_group, GroupName).

get_request_ip() -> get(ewg_request_client_ip).
get_request_port() -> get(ewg_request_client_port).
