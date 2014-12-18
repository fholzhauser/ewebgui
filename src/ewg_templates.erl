-module(ewg_templates).
-export([crash_report/1, no_access/1, no_access/0, no_access_content/0, no_access_content/1, no_page/0, no_page/1, page/1, page/2, home/0]).
-import(ewg_lib, [plget/2]).

-define(CSS, [
    "/css/menuV2.css",
    "/js/jquery-ui-1.10.3.custom/css/start/jquery-ui-1.10.3.custom.css",
    "/css/jquery-ui-timepicker-addon.css",
    "/css/ewebgui.css"
]).
-define(JS, [
    "/js/jquery-1.10.2.min.js",
    "/js/jquery-ui-1.10.3.custom/js/jquery-ui-1.10.3.custom.min.js",
    "/js/jquery-ui-timepicker-addon.min.js",
    "/js/menuV2.js",
    "/js/jquery.flot.min.js",
    "/js/ewebgui.js"
]).


crash_report(Info) -> page([
    {'div', [{class, "padding10"}], [
        {h1, [], ["Something went wrong, the page crashed."]},
        hr, "Support information:", hr,
        {pre, [], [io_lib:format("~150p", [Info])]}
    ]}
], [{title, "Crash Report !"}]).

no_page() ->
    no_page(undefined).
no_page(Info) -> page([
    {'div', [{class, "padding10"}], [
        {h1, [], "There is no page configured on the requested URL !"},
        hr, "If you think that the URL is correct, please contact support.", hr,
        if Info == undefined -> []; true -> ["Support info: ", Info] end
    ]}
], [{title, "Page not found !"}, {subhead, "Page not found !"}]).

no_access_content() ->
    no_access_content(ewg_access:get_user()).

no_access_content(UserName) ->
    {[
        {'div', [{class, "padding10"}], [
            {h1, [], "No permission to access this page"},
            hr, "User ", {b, [], [UserName]}, " has no access to this page.", hr
        ]}
    ], [{title, "Access denied !"}, {subhead, "Access Denied !"}]}.

no_access() ->
    no_access(ewg_access:get_user()).

no_access(UserName) -> page([
    {'div', [{class, "padding10"}], [
        {h1, [], "No permission to access this page"},
        hr, "User ", {b, [], [UserName]}, " has no access to this page.", hr
    ]}
], [{title, "Access denied !"}, {subhead, "Access Denied !"}]).

home() ->
    % home screen content
    {
        [     hr,
        {h1, [{class, "center"}
    ], "Welcome to O&M GUI !"}, hr], [{title, "Home"}, {subhead, "Home"}]}.

%% common html head for all pages
head(Opt) ->
    Title = case plget(title, Opt) of
        undefined -> "";
        T -> [{title, [], [T]}]
    end,
    Js = [
        {script, [
            {type, "text/javascript"},
            {charset, "utf-8"},
            {src, JSSrc}
        ], []} ||
        JSSrc <- case plget(js, Opt) of
            undefined -> ?JS;
            ExtraJs -> ?JS ++ ExtraJs
        end
    ],
    Css = [
        {link, [
            {rel, "stylesheet"},
            {href, CSSSrc}
        ]} ||
        CSSSrc <- case plget(css, Opt) of
            undefined -> ?CSS;
            ExtraCss -> ?CSS ++ ExtraCss
        end
    ],
    {head, [], [
        {meta, [{'http-equiv', "cache-control"}, {content, "no-cache"}]},
        {link, [{rel, "shortcut icon"}, {href, "/images/favicon.ico?v=2"}]},
        Js, Css, Title
    ]}.
%% main page rendering:
%% Beside the ehtml Content that is generated by the target module the following
%% options are supported:
%%
%% {js, ["/js/...", "/js/...", ...]} - list of extra javascript files the page needs
%% {css, ["/css/...", "/css/...", ...]} - list of extra css files the page needs
%% {title, Title} - Title text to be displayed in the browser tab
%% {mainhead, MainHead} - Main title of the page to be displayed in the top header
%% {subhead, SubHead} - Sub title of the page to be displayed in the top header
%% {layout, login|normal} - login page should not display 2 columns if this
%%                          option is not set, defaults to normal

page({Content, Opt}) ->
    page(Content, Opt);

page(Content) ->
    page(Content, []).

page(Content, Opt) ->
    ewg_html:r(
        [
            "<!DOCTYPE html>",
            {html, [], [
                head(Opt),
                {body, [], [
                    %% the main div, interesting for the 100% layout
                    {'div', [{id, "page"}], [
                        %% page header
                        {'div', [{id, "header"}], [
                            {img, [{src, "/images/logo.jpg"}]},
                            {span, [{id, "header_main"}], [
                                case plget(mainhead, Opt) of
                                    undefined ->
                                        ewg_conf:read(default_main_header, "O&M");
                                    MainHead ->
                                        MainHead
                                end
                            ]},
                            case plget(subhead, Opt) of
                                undefined ->
                                    [];
                                SubHead ->
                                    {span, [{id, "header_sub"}], [SubHead]}
                            end,
                            case ewg_access:get_user() of
                                undefined -> [];
                                UserName ->
                                    {
                                        'div',
                                        [{id, "header_user_name"}],
                                        [
                                            {span, [], "user : "}, UserName, br,
                                            {
                                                form,
                                                [{method, "post"}],
                                                [{macro, {action_postback, "ewg_logout", [{'div', [{id, "header_logout_button"}, {class, "center"}], "logout"}]}}]
                                            }
                                        ]
                                    }
                            end
                        ]},
                        %% normally there is a 2 column layout except for the login page
                        %% it is controlled by the layout option
                        case plget(layout, Opt) of
                            noleft -> [];
                            _ ->
                                {'div', [{id, "left"}], [ewg_menu:show(), ewg_status:show()]}
                        end,
                        {'div', [{id, "content"}], [Content]}
                    ]}
                ]}
            ]}
        ]
    ).
