-module(ewg_appmod).
-include_lib("yaws/include/yaws_api.hrl").
-include_lib("yaws/include/yaws.hrl").
-export([out/1, list_sessions/0, delete_session/1]).
-import(ewg_lib, [plget/2, plget/3, plset/3, cookie_name/0]).


-record(session_data, {
    user,
    ip,
    opaque = [],
    %% to apply the PRG (Post/Redirect/Get) pattern here we can store data that
    %% survives redirect
    redirect_data
}).

out(A) ->
    Params = case (A#arg.req)#http_request.method of
        'GET' ->
            yaws_api:parse_query(A);
        'POST' ->
            yaws_api:parse_post(A);
        _ ->
            []
    end,
    Path = case A#arg.pathinfo of undefined -> ""; P -> P end,
    {ClientIp, _ClientPort} = A#arg.client_ip_port,
    %% 4 nested cases are ugly, yet I think here it is more readable than making small functions
    case yaws_api:find_cookie_val(cookie_name(), (A#arg.headers)#headers.cookie) of
        [] ->
            %% no cookie
            new_session(ClientIp);
        Cookie ->
            %% cookie, check server session
            case yaws_api:cookieval_to_opaque(Cookie) of
                {ok, S} when S#session_data.user == undefined ->
                    %% no session user (yet) could be login form post
                    case [plget(P, Params) || P <- ["login_form_submit", "username", "password"]] of
                        [undefined, _, _] ->
                            %% no it's not, pity ..."
                            [{html, login_screen()}];
                        [_, User, Pass] ->
                            case ewg_access:check_user_login(User, Pass) of
                                ok ->
                                    put(ewg_user, User),
                                    yaws_api:replace_cookie_session(Cookie, #session_data{user = User, ip = ClientIp}),
                                    %ewg_user_log:log("user login", "", "success"),
                                    %% valid login: LET'S HANDLE THE REQUEST !!!
                                    %% don't give the login parameters to the handler but if it is
                                    %% a GET request then use the query parameters instead
                                    [redirect(Cookie, [], Path)];
                                _ ->
                                    %% bad credentials, try again
                                    [{html, login_screen("Login Failed !")}]
                            end
                    end;
                {ok, S} ->
                    %% there is a session user
                    put(ewg_user, S#session_data.user),
                    %% who might just submit a logout
                    case plget("ewg_hidden_action", Params) of
                        "ewg_logout" ->
                            %ewg_user_log:log("user logout", "", "success"),
                            % delete the session and make a new one
                            yaws_api:delete_cookie_session(Cookie),
                            erase(ewg_user),
                            new_session(ClientIp);
                        _ ->
                            % no logout : LET'S HANDLE THE REQUEST !!!
                            [handle_request(Cookie, Path, Params)]
                    end;
                _ ->
                    new_session(ClientIp)
            end
    end ++
    %% do whatever it takes to make sure that browsers don't cache generated page
    %% inets is smarter here there it is configurable not to cache esi pages.
    %% without this back/forward buttons can do nice things (like re-login after logout)
    [
        {header, {cache_control, "no-cache, no-store, must-revalidate"}},
        {header, {expires, "0"}}
    ].

handle_request(Cookie, Path, Params) ->
    case catch(ewg_apps:request(Path, Params)) of
        {'EXIT', Reason} ->
            ewg_event:event([
                {ewg_event_id, page_crash},
                {crash_reason, Reason},
                {stacktrace, erlang:get_stacktrace()}
            ]),
            {html, ewg_templates:crash_report([{reason, Reason}, {input, Params}])};
        {error, app_callback_not_found} ->
            {html, ewg_templates:no_page("App callback not found")};
        {error, app_handler_not_found} ->
            {html, ewg_templates:no_page("Request handler not found")};
        {error, no_access} ->
            {html, ewg_templates:no_access()};
        redirect ->
            redirect(Cookie, [], Path);
        {redirect, Data} ->
            redirect(Cookie, Data, Path);
        {redirect, Data, NewPath} ->
            redirect(Cookie, Data, NewPath);
        {Content, Options} ->
            {html, ewg_templates:page(Content, Options)};
        Content ->
            {html, ewg_templates:page(Content, [])}
    end.

redirect(SessionId, RedirData, Path) ->
    {ok, S} = yaws_api:cookieval_to_opaque(SessionId),
    yaws_api:replace_cookie_session(SessionId, S#session_data{redirect_data = RedirData}),
    {redirect, Path}.

new_session(ClientIp) ->
    Cookie = yaws_api:new_cookie_session(
        #session_data{
            ip = ClientIp
        }
    ),
    [
        %% Page data
        {html, login_screen()},
        %% Cookie header
        yaws_api:setcookie(cookie_name(), Cookie, "/")
    ].


login_screen() -> login_screen("").
login_screen(Notification) ->
    ewg_templates:page([
        {form, [{method, "post"}, {autocomplete, "off"}], [
            {'div', [{id, "login"}], [
                "User Name", br,
                {macro, {input_text, "username"}}, br,
                "Password", br,
                {macro, {input_password, "password"}}, br,
                {input, [{type, "submit"}, {value, "Login"}, {name, "login_form_submit"}]},
                br, br, Notification
            ]}
        ]}
    ], [
        {layout, noleft}, {subhead, "Login"}, {title, "Login"}
    ]).

list_sessions() ->
    %% This is probably against the intention of the yaws developers as
    %% the record is not exposed in any of the includes. Anyway, seems handy.
    [
        {Cookie, Start, Opaque#session_data.user, Opaque#session_data.ip} || {
            ysession, Cookie, _GregTimeToDie, _InactivityTimer,
            Start, _NotifyPid, Opaque
        } <- yaws_session_server:list()
    ].

delete_session(Cookie) ->
    yaws_api:delete_cookie_session(Cookie).
