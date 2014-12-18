%% This module generates HTML from a data structure without pretty printing
%% e.g
%% ewg_html:r({'div', [{class, "div_class"}], [
%%     {h1, [], "Hello"},
%%     br,
%%     {hr, [{class, "supersep"}]},
%%     1
%% ]}).
%% produces:
%% "<div class=\"div_class\"><h1>Hello</h1><br/><hr class=\"supersep\"/>1</div>"

-module(ewg_html).
-export([r/1, render/1]).
-import(ewg_lib, [tost/1]).

%% These are HTML void elements (that are not allowed to have content), should
%% be self closing e.g. <br /> and they can have attributes
-define(HTML_VOID, [
    area, base, br, col, command, embed, hr, img, input, keygen,
    link, meta, param, source, track, wbr
]).

render(Data) ->
    r(Data).

r({m, Data}) ->
    r({macro, Data});
r({macro, Data}) ->
    r(ewg_html_macros:r(Data));

r({mfa, M, F, A}) ->
    apply(M, F, A);

%% render single element tuple unless it is undefined
r(undefined) ->
    r("");
r(Tag) when is_atom(Tag) ->
    r({Tag});
r({Tag}) ->
    r({Tag, [], []});
r({Tag, Attrs}) ->
    r({Tag, Attrs, []});
r({Tag, Attrs, Content}) ->
    list_to_binary([
        %% opening
        "<", tost(Tag),
            %% attributes
            [case Attr of
                {A, V} ->
                    [" ",  tost(A),  "=\"",  tost(V),  "\""];
                Attr ->  [" ",  tost(Attr),  " "]
            end || Attr <- Attrs]
        ,
        case lists:member(Tag, ?HTML_VOID) of
            true ->
                %% void element, close it with the /> syntax sugar
                "/>";
            false ->
                %% normal element with content
                [">" , r(Content),  "</" , tost(Tag) , ">"]
        end
    ]);

%% now we need to match the list of element scenario naively assuming
%% that if the first element is a tuple, atom or list then it is definitely
%% a list of elements
r([Elem | Rest]) when is_list(Elem); is_tuple(Elem); is_atom(Elem) ->
    [r(E) || E <- [Elem | Rest]];

%% at the end assume that the only thing left is a text string or anything
%% we could/should stringify
r(Elem) ->
    tost(Elem).
