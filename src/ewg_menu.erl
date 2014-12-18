-module(ewg_menu).
-export([show/0, show/2]).

%% this module works with this js menu solution: http://www.menucool.com/vertical/vertical-menu

show() ->
    show("menuV2", "menuV2").

show(Class, Id) ->
    render(Class, Id, ewg_apps:make_menu_structure()).

render(Class, Id, Menu) ->
    {ul, [{class, Class}, {id, Id}], render_items(Menu, [])}.
    
render_items([{Text, Path, []} | Rest], Acc) ->
    render_items(Rest, Acc ++ [{li, [], [{a, [{href, Path}], Text}]}]);

render_items([{Text, _Path, SubMenus} | Rest], Acc) ->
    render_items(Rest, Acc ++ [
        {li, [], [
            {a, [{href, "#"}], Text }, 
            {ul, [{class, "sub"}], render_items(SubMenus, [])}
        ]}]
    );

render_items([], Acc) ->
    Acc.


