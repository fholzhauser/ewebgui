-module(ewg_admin_users).
-import(ewg_access, [get_form_param/1, set_form_param/2, set_form_params/1]).
-compile(export_all).

-include_lib("ewebgui/include/ewebgui.hrl").

webgui_info() -> [
    {handlers, [
        {"/change_own_pw", {
            change_own_pw,
            "Change own PW",
            {route, [
                {
                    ["old_pw", "new_pw", "new_pw_confirm"],
                    {
                        [
                            {"old_pw", mandatory, [
                                fun({_, OldPW}, _Params) ->
                                    case ewg_access:check_user_login(ewg_access:get_user(), OldPW) of
                                        ok -> {ok, OldPW};
                                        _ ->
                                            {error, "Incorrect current password"}
                                    end
                                end
                            ]},
                            {"new_pw", mandatory, [
                                {not_same_as, "old_pw", "Please use a different password than the current one"},
                                %% this regex with lookahead is good to remember for password format checking.
                                {regex, "^(?=.*[0-9])(?=.*[A-Z])(?=.*[a-z]).{6,10}$", "The new password should be between 6 and 10 character long and must contain at least 1 digit and 1 capital letter."}
                            ]},
                            {"new_pw_confirm", mandatory, [{same_as, "new_pw", "Please type your new password twice for confirmation"}]}
                        ],
                        {ewg_admin_users, change_own_pw}
                    }
                },
                {[], {[], {ewg_admin_users, change_own_pw_form}}}
            ]}
        }},
        {"/users", { %% path under the application path (which is configured in ewg_apps:store_app e.g. ewg_apps:store_app({"/admin", ewg_cb_admin}))
            users, %% required permission for this handler
            "Users", %% menu text, if menu is not required for this handler then is should be set to the atom : nomenu
            {handlers, [
                {"/add", {
                    add,
                    "Add",
                    {route, [ %% route: multiple actions on the same path depending on matching input parameters
                        {
                            ["user_name", "add_user"], %% request parameters to match for this route (see ewg_apps:match_route)
                            { %% validations to perform on parameters (see ewg_lib:validate)
                                [{"user_name", mandatory, []}],
                                {ewg_admin_users, add_user} %% callback {M, F} call M:F(ValidatedParams, ValidationErrors, RemainingPermissions, RemainingPath)
                            }
                        },
                        {
                            [], %% request parameters to match for this route [] means this is the default
                            {[], {ewg_admin_users, add_user_form}}
                        }
                    ]}
                }},
                {"/list", {
                    list,
                    "List",
                    {route, [
                        {
                            ["ewg_hidden_item_id", {"ewg_hidden_action", "delete"}],
                            {[], {ewg_admin_users, delete_user}}
                        },
                        {
                            ["ewg_hidden_item_id", {"ewg_hidden_action", "reset_pw"}],
                            {[], {ewg_admin_users, reset_pw}}
                        },
                        {
                            ["ewg_hidden_item_id", {"ewg_hidden_action", "edit"}],
                            {[], {ewg_admin_users, edit_user_form}}
                        },
                        {
                            ["edit_user", "user_name", "group"],
                            {[], {ewg_admin_users, edit_user}}
                        },
                        {[], {[], {ewg_admin_users, list_users}}}
                    ]}
                }}
            ]}
        }}
    ]},
    {permissions, [
        {change_own_pw, "Change Own PW", "Change own password"},
        {users, "Users", "User Administration", [
            {add, "Add", "Add New users"},
            {list, "List", "List users"},
            {[list, edit], "Edit User", "Edit user information"},
            {[list, reset_pw], "Reset Pw", "Reset User Password"},
            %{[list, view_all], "View All", "View all users, not only those in the managed groups"},
            {[list, delete], "Delete", "Delete user"}
        ]}
    ]}
].

change_own_pw(validated, _Permissions) ->
    ewg_access:set_user_var(password, get_form_param("new_pw")),
    {[
        {h2, [], "Your password is updated"},hr
        %{form, [{method, "POST"}, {action, "/"}], [
            %{input, [{type, "submit"}, {value, "Back to home"}]}
        %]}
    ],[
        {subhead, "Admin - Users"},
        {title, "Admin - Users"}
    ]};

change_own_pw(validation_failed, _Permissions) ->
    change_own_pw_form_int().

change_own_pw_form(validated, _Permissions) ->
    change_own_pw_form_int().

change_own_pw_form_int() ->
    {[
        {h2, [], "Change your own password"},hr,
        {macro, {page_help, "Change your own password"}},
        {macro, {eform, [], [
            {"Current Password ", {macro, {input_password, "old_pw"}}},
            {"New Password ", {macro, {input_password, "new_pw"}}},
            {"Confirm new password ", {macro, {input_password, "new_pw_confirm"}}}
        ], [
            {input, [{type, "submit"}, {value, "Change Password"}, {name, "change_own_pw"}]}
        ]}}
    ],[
        {subhead, "Admin - Change Password"},
        {title, "Admin - Change Password"}
    ]}.

list_users(validated, Permissions) ->
    [ResetP, DeleteP, EditP, ViewAllP] = [lists:member(Tag, Permissions) || Tag <- [reset_pw, delete, edit, view_all]],
    Users = lists:sort([
        [User | [ewg_access:get_user_var(User, Tag) || Tag <- [group, name, email]]] || User <- ewg_access:list_users(),
        ViewAllP orelse is_managed_group(ewg_access:get_user_var(User, group))
    ]),
    {[
        {h2, [], "List GUI users"},hr,
        {macro, {page_help, "List and manage users"}},
        {form, [{method, "POST"}], [
            {macro, {datatable,
                ["User", "Group", "Name", "E-Mail"] ++
                if EditP -> ["Edit"]; true -> [] end ++
                if ResetP -> ["Reset PW"]; true -> [] end ++
                if DeleteP -> ["Delete"]; true -> [] end,
                lists:map(
                    fun([User, Group | Rest]) ->
                        [User, Group | Rest] ++
                        case {is_managed_group(Group), EditP} of
                            {true, true} ->
                                [{p, [{class, "center"}], {macro, {
                                    action_postback, User,
                                    "edit",
                                    [{img, [{src, "/images/edit.gif"}]}]
                                }}}];
                            {false, true} -> [br];
                            _ -> []
                        end ++
                        case {is_managed_group(Group), ResetP} of
                            {true, true} ->
                                [{p, [{class, "center"}], {macro, {
                                    action_postback, User,
                                    "reset_pw",
                                    [{img, [{src, "/images/change-password-icon.png"}]}]
                                }}}];
                            {false, true} -> [br];
                            _ -> []
                        end ++
                        case {is_managed_group(Group), DeleteP} of
                            {true, true} ->
                                [{p, [{class, "center"}], {macro, {
                                    delete_postback, User,
                                    "Are you sure you want to delete user : " ++ User ++ " ?"
                                }}}];
                            {false, true} -> [br];
                            _ -> []
                        end
                    end,
                    Users
                ),
                []
            }}
        ]}
    ],[
        {subhead, "Admin - Users"},
        {title, "Admin - Users"}
    ]}.

delete_user(validated, Permissions) ->
    %% make sure the submitter has permission to do this since this function is routed under the "list" path
    %% at the moment there is no automated permission check for this under the check done for the path (list permission in this case)
    User = get_form_param("ewg_hidden_item_id"),
    case {
        is_managed_group(ewg_access:get_user_var(User, group)),
        lists:member(delete, Permissions)
    } of
        {true, true} ->
            ewg_access:delete_user(User),
            ?EVENT(user_deleted, [{result, success}, {target_user, User}]),
            list_users(validated, Permissions);
        _ ->
            ewg_templates:no_access_content()
    end.

reset_pw(validated, Permissions) ->
    User = get_form_param("ewg_hidden_item_id"),
    case {
        is_managed_group(ewg_access:get_user_var(User, group)),
        lists:member(reset_pw, Permissions)
    } of
        {true, true} ->
            Pw = ewg_lib:pwgen(),
            ewg_access:set_user_var(User, password, Pw),
            {[
                {h2, [], "Reset GUI password for user : " ++ User},hr,
                {macro, {page_help, "Reset password, the system offers an auto generated password"}},
                "The new login password for the user is : ", {b, [{style, "color:yellow;"}], Pw}, hr,
                {form, [], [
                    {input, [{type, "submit"}, {value, "Back to User List"}]}
                ]}
            ],[
                {subhead, "Admin - Users"},
                {title, "Admin - Users"}
            ]};
        _ ->
            ewg_templates:no_access_content()
    end.

edit_user(validated, Permissions) ->
    User = get_form_param("user_name"),
    case {
        is_managed_group(ewg_access:get_user_var(User, group)),
        lists:member(edit, Permissions)
    } of
        {true, true} ->
            OldParams = [{N, ewg_access:get_user_var(User, N)} || N <- [group, name, email, mobile, description]],
            NewParams = [
                {group, get_form_param("group")},
                {name, get_form_param("name")},
                {email, get_form_param("email")},
                {mobile, get_form_param("mobile")},
                {description, get_form_param("description")}
            ],
            [ewg_access:set_user_var(User, Param, Value) || {Param, Value} <- NewParams],
            if
                OldParams =/= NewParams ->
                    ?EVENT(user_modified, [
                        {target_user, User},
                        {result, success},
                        {changes, ewg_lib:plmods(OldParams, NewParams)}
                    ]);
                true ->
                    ok
            end,
            edit_user_form_int("User information updated for " ++ User);
        _ ->
            ewg_templates:no_access_content()
    end;

edit_user(validation_failed, _Permissions) ->
    User = get_form_param("ewg_hidden_item_id"),
    edit_user_form_int("User information update failed for " ++ User).

edit_user_form(validated, _Permissions) ->
    User = get_form_param("ewg_hidden_item_id"),
    set_form_params([
        {"user_name", User} |
        [{Field, ewg_access:get_user_var(User, Data)} || {Field, Data} <- [
            {"name", name}, {"email", email}, {"group", group},
            {"mobile", mobile}, {"description", description}
        ]]
    ]),
    edit_user_form_int("Edit GUI user information for " ++ User).

edit_user_form_int(Hdr) ->
    {[
        {h2, [], Hdr},hr,
        {macro, {page_help, "Edit GUI user"}},
        {macro, {eform, [], [
            {"Name ", {macro, {input_text, "name"}}},
            {"E-mail ", {macro, {input_text, "email"}}},
            {"Mobile ", {macro, {input_text, "mobile"}}},
            {"Description ", {macro, {input_text, "description"}}},
            {"Group", {macro, {input_select, "group", [{G, G} || G <- ewg_access:list_groups(), is_managed_group(G)]}}}
        ], [
            {macro, {input_hidden, "user_name"}},
            {input, [{type, "submit"}, {value, "Update User"}, {name, "edit_user"}]}
        ]}},
        {form, [{action, "list"}], [{input, [{type, "submit"}, {value, "Back to User List"}]}]}
    ],[
        {subhead, "Admin - Edit User"},
        {title, "Admin - Edit User"}
    ]}.

add_user(validated, _Permissions) ->
    UserName = get_form_param("user_name"),
    Pw = ewg_lib:pwgen(),
    Group = get_form_param("group"),
    Params = [
        {group, Group},
        {name, get_form_param("name")},
        {email, get_form_param("email")},
        {mobile, get_form_param("mobile")},
        {description, get_form_param("description")}
    ],
    Res = ewg_access:add_user(UserName, Pw, Params),
    if
        Res == ok ->
            ?EVENT(user_added, [
                {target_user, UserName},
                {result, success},
                {user_data, Params}
            ]),
            add_user_form_int(
                "User: " ++ UserName ++ " added",
                ["The password of the new user is : ", {b, [{style, "color:yellow;"}], Pw}, hr]
            );
        true ->
            ewg_access:add_validation_result("user_name", "This username exists already"),
            add_user_form_int(
                "Failed to add user " ++ UserName ++ ": username already exists !",
                ""
            )
    end;

add_user(validation_failed, _Permissions) ->
    add_user_form_int("Failed adding user. Please check your input!", "").

add_user_form(validated, _Permissions) ->
    add_user_form_int().

add_user_form_int() ->
    add_user_form_int("Add GUI user", "").
add_user_form_int(Header, InfoText) ->
    {[
        {h2, [], Header},hr,
        {macro, {page_help, "Add new GUI user"}},
        InfoText,
        {macro, {eform, [], [
            {"User Name ", {macro, {input_text, "user_name"}}},
            {"Name ", {macro, {input_text, "name"}}},
            {"E-mail ", {macro, {input_text, "email"}}},
            {"Mobile ", {macro, {input_text, "mobile"}}},
            {"Description ", {macro, {input_text, "description"}}},
            {"Group", {macro, {input_select, "group", [{G, G} || G <- ewg_access:list_groups(), is_managed_group(G)]}}}
        ], [
            {input, [{type, "submit"}, {value, "Add User"}, {name, "add_user"}]}
        ]}},
        {form, [{action, "list"}], [{input, [{type, "submit"}, {value, "Back to User List"}]}]}
    ],[
        {subhead, "Admin - Add User"},
        {title, "Admin - Add User"}
    ]}.

is_managed_group(Group) ->
    case ewg_access:get_group_var(webadmin_managed_groups) of
        all -> true;
        undefined -> false;
        Groups ->
            lists:member(Group, Groups)
    end.
