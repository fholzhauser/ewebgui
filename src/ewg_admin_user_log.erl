-module(ewg_admin_user_log).
-export([webgui_info/0, list/2]).

-import(ewg_lib, [plget/2, plget/3, plset/3, mplset/2, tost/1]).
-import(ewg_access, [get_form_params/0, get_form_param/1, set_form_params/1, get_username/0]).

webgui_info() -> [
    {handlers, [
        {"/audit_log", {
            audit_log,
            "Audit Log",
            {[], {ewg_admin_user_log, list}}
        }}
    ]},
    {permissions, [
        {audit_log, "View Audit Log", "View Audit Log of user actions"}
    ]}
].

list(_Validation, _Permissions) ->
    Search = ewg_access:get_form_param("search"),
    {[
        {h2, [], "User Audit Log"}, hr,
        {macro, {page_help, [
            "Log view of user actions. The logs are searchable by considering the log entry as one concatenated text string.",
            " Typing any part of the text will result in all the entries that contain the particular string. In the search ",
            "term it is possible to use wildcard expressions (i.e. regular expressions).\n\nexample:\n\n <b>.*</b> matches ",
            "any part of the string. A search could be : <b> 08:.*admin.*clone.*7673882</b> could be used to search the logs ",
            " for clone actions by the admin user for a number that contains the digits 7673882 and done between 8 and 9 o'clock",
            "\n\n. Please note that the GUI logs contain max 50000 entries (e.g. approx 250 days with 200 entries a day). ",
            "Logs are also written into an encrypted logfile that can be retrieved by operations if necessary. The search ",
            "results have a limit of max 2000 entries to keep the GUI responsive. If you get too many results, ",
            "please repeat the search with more restrictive conditions to make sure it fits in the result set."
        ]}},
        {form, [{method, "POST"}], [
            {macro, {formtable, [
                {
                    "Search",
                    [{macro, {input_text, "search"}}],
                    [
                        {hint, [
                            "Free text/regular expression search in user logs"
                        ]}
                    ]
                }
            ]}},
            {input, [{type, "submit"}, {value, "Search"}, {name, "search_button"}]},
            hr,

            {macro, {datatable,
                ["Timestamp", "User", "Application", "Action", "Target", "Result", "Details"],
                lists:reverse(lists:sort(ewg_user_log:search_gui(Search)))
            }}
        ]}
    ],[
        {subhead, "Admin - Audit Log"},
        {title, "Admin - Audit Log"}
    ]}.

