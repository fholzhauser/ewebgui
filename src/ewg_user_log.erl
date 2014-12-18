-module(ewg_user_log).
-export([
    log/0, log/3, log/4,
    set_target/1, set_action/1, set_application/1, set_details/1,
    set_result/1, search/1, search_gui/1
]).
-import(ewg_lib, [tost/1]).

-define(RESULT_LIMIT, 2000).
-define(DB_LIMIT, 50000).

%% This logging module uses the process dictionary. Some say it is not
%% good since in transfers data in "stealth" not directly visible to
%% the request handler functions. IMO there is nothing wrong with process
%% dictionary.
%% It is all matter of awareness among developers touching the code
%% (hence this little text here). Anyone is welcome of course to argue
%% and come up with an alternative solution that is similarly non intrusive
%% in the main request handler code.

-record(user_log, {
    timestamp,
    userid,
    path,
    application,
    call,
    action,
    target,
    result,
    details
}).

set_target(Target) ->
    put(ewg_current_target, Target).

set_result(Result) ->
    put(ewg_current_result, Result).

set_application(Application) ->
    put(ewg_current_appname, Application).

set_action(Action) ->
    put(ewg_current_action, Action).

set_details(Details) ->
    put(ewg_current_details, Details).

%% This is a low performance audit trail facility to store the user activity
log() ->
    log(
        get(ewg_current_action),
        get(ewg_current_target),
        get(ewg_current_result),
        get(ewg_current_details)
    ).

log(Action, Target, Result) ->
    log(Action, Target, Result, []).

log(Action, Target, Result, Details) ->
    Rec = #user_log{
        timestamp = os:timestamp(),
        userid = ewg_access:get_user(),
        path = get(ewg_current_path),
        application = get(ewg_current_appname),
        call = get(ewg_current_call),
        action = Action,
        target = Target,
        result = Result,
        details = Details
    },
    case catch write_log(ewg_user_log, Rec) of
        {'EXIT',{aborted,{no_exists, ewg_user_log}}} ->
            mnesia:create_table(ewg_user_log, [
                {disc_copies, mnesia:system_info(db_nodes)},
                {record_name, user_log},
                {type, ordered_set},
                {attributes, record_info(fields, user_log)}
            ]),
            write_log(ewg_user_log, Rec);
        _ ->
            clean_if_needed(),
            ok
    end.

search_gui(Search) ->
    case ets:info(ewg_user_log) of
        undefined -> [];
        _ ->
            [format_for_gui(R) || R <-search(Search)]
    end.


search(Search) ->
    case ets:last(ewg_user_log) of
        '$end_of_table' -> [];
        Key ->
            [Log] = ets:lookup(ewg_user_log, Key),
            case match(Log, Search) of
                true ->
                    search([Log], Key, 1, Search);
                false ->
                    search([], Key, 1, Search)
            end
    end.

search(Found, _, ?RESULT_LIMIT, _) -> Found;

search(Found, LastKey, Count, Search) ->
    case ets:prev(ewg_user_log, LastKey) of
        '$end_of_table' -> Found;
        Key ->
            [Log] = ets:lookup(ewg_user_log, Key),
            case match(Log, Search) of
                true ->
                    search([Log | Found], Key, Count+1, Search);
                false ->
                    search(Found, Key, Count+1, Search)
            end
    end.


match(_Rec, "") -> true;
match(_Rec, undefined) -> true;
match(Rec, String) ->
    case catch re:run(format_line(Rec), String, [{capture, none}, caseless]) of
        match -> true;
        _ -> false
    end.

format_line(#user_log{
    timestamp = TS,
    userid = User,
    application = App,
    action = Action,
    target = Target,
    result = Result,
    details = Details
}) ->
    DetailsStr = string:join([kvtost(D) || D <- Details], ","),
    logstring([format_logtime_local(TS), User, App, Action, Target, Result]++[DetailsStr]).

format_for_gui(#user_log{
    timestamp = TS,
    userid = User,
    application = App,
    action = Action,
    target = Target,
    result = Result,
    details = Details
}) ->
    DetailsStr = string:join([kvtost(D) || D <- Details], ","),
    [format_logtime_local(TS), tost(User), tost(App), tost(Action), tost(Target), tost(Result)]++[DetailsStr].

write_log(Table, #user_log{
    userid = User, action = Action, application = App, target = Target,
    result = Result, details = Details
} = Rec) ->
    ewg_event:event([
        {ewg_event_id, user_log}, {user_id, User}, {action, Action},
        {application, App}, {target, Target}, {result, Result},
        {detail, Details}
    ]),
    mnesia:dirty_write(Table, Rec).

clean_if_needed() ->
    DBSize = ets:info(ewg_user_log, size),
    if
        DBSize > ?DB_LIMIT -> clean(DBSize - ?DB_LIMIT);
        true -> ok
    end.

clean(0) ->
    ok;
clean(N) ->
    % it is an ordered_set table, first is always oldest ..
    case ets:first(ewg_user_log) of
        '$end_of_table' ->
            ok;
        Key ->
            ets:delete(ewg_user_log, Key),
            clean(N-1)
    end.


format_logtime_local({N1, N2, N3}) ->
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_local_time({N1, N2, N3}),
    lists:flatten(
        io_lib:format(
            "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
            [Y, M, D, H, Min, S, round(N3/1000)]
        )
    ).

logstring(Terms) ->
    string:join([iofmtnl(T) || T <- Terms], ", ").

iofmt(Term) ->
    case io_lib:char_list(Term) of
        true ->
            Term;
        _ ->
            lists:flatten(io_lib:format("~p", [Term]))
    end.

iofmtnl(Term) ->
    [C || C <- iofmt(Term), C =/= $\n].

kvtost({K,V}) ->
    ewg_lib:tost(K) ++ "=" ++ iofmtnl(V).
