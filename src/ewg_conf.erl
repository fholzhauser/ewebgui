-module(ewg_conf).
-export([read/1, read/2, write/2]).

%Change this if you want to plug in your own config things. Natively it
%uses mnesia.

read(Key) ->
    read(Key, undefined).

read(Key, Default) ->
    case mnesia:dirty_read(ewg_conf, Key) of
        [] ->
            Default;
        [{_, Key, Value}] ->
            Value
    end.

write(Key, Value) ->
    mnesia:dirty_write({ewg_conf, Key, Value}).

