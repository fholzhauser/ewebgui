-module(ewg_kibana).
-export([webgui_info/0, show_kibana/2]).

webgui_info() -> [
    {handlers, [
        {"/kibana", {kibana, "Dashboard", {[], {?MODULE, show_kibana}}}}
    ]},
    {permissions, [
        {kibana, "Dashboard", "Show Dashboard"}
    ]}
].

show_kibana(_, _) ->
    {
        [{iframe, [{class, "fullwidth fullheight"}, {src, "/js/kibana-3.1.2/index.html"}]}],
        [{layout, fillheight}]
    }.
