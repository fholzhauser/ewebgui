-module(ewg_event).
-export([event/1, list_callbacks/0, delete_callback/1, register_callback/1, reset_callbacks/0]).

event(EventData) ->
    Data = complement_data(EventData),
    CallBacks = ewg_conf:read(ewg_event_callbacks, []),
    [
        case CB of
            {M, F} -> erlang:spawn(M, F, [Data]);
            {M, F, A} -> erlang:spawn(M, F, [Data | A]);
            Fun when is_function(Fun) -> erlang:spawn(fun() -> Fun(Data) end)
        end || CB <- CallBacks
    ],
    ok.

list_callbacks() ->
    ewg_conf:read(ewg_event_callbacks, []).

delete_callback(CB) ->
    CallBacks = ewg_conf:read(ewg_event_callbacks, []),
    ewg_conf:write(ewg_event_callbacks, lists:delete(CB, CallBacks)).

register_callback({M, F}) -> add_callback({M, F});
register_callback({M, F, A}) -> add_callback({M, F, A});
register_callback(Fun) when is_function(Fun)-> add_callback(Fun);
register_callback(_) -> {error, invalid_callback_format}.

add_callback(CB) ->
    CallBacks = ewg_conf:read(ewg_event_callbacks, []),
    case lists:member(CB, CallBacks) of
        false -> ewg_conf:write(ewg_event_callbacks, [CB | CallBacks]);
        true -> ok
    end,
    ok.

reset_callbacks() ->
    ewg_conf:write(ewg_event_callbacks, []).

complement_data(Data) ->
    [
        {ewg_event_timestamp, os:timestamp()},
        {request_user, ewg_access:get_user()},
        {request_ip, ewg_access:get_request_ip()}
    | Data].
