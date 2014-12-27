-module(ewg_conf).
-export([read/1, read/2, write/2]).
-include_lib("ewebgui/include/ewebgui.hrl").

%Change this if you want to plug in your own config things. Natively it
%uses mnesia.

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

