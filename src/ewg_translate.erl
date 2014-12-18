-module(ewg_translate).
-export([strict/2]).

strict(Language, Text) ->
    case ets:lookup(ewg_translations, {Language, Text}) of
        [] -> Text;
        [Translation] -> Translation
    end.

