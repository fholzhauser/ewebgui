-module(ewg_conf).
-export([read/1, read/2, write/2, list/0]).
-include_lib("ewebgui/include/ewebgui.hrl").

read(Key) ->
    read(Key, undefined).

read(Key, Default) ->
    case ets:lookup(ewg_conf, Key) of
        [] ->
            Default;
        [{_, Key, Value}] ->
            Value
    end.

write(Key, Value) ->
    case read(Key) of
        Value ->
            ok;
        OldValue ->
            mnesia:dirty_write({ewg_conf, Key, Value}),
            if
                Key =/= apps ->
                    ?EVENT(ewg_config_change, [{config_key, Key}, {old_value, OldValue}, {new_value, Value}]);
                true -> ok
            end
    end.

list() -> [{Key, Val} || {_, Key, Val} <- ets:tab2list(ewg_conf), Key =/= apps].
