-module(ewg_admin_groups).
-include_lib("ewebgui/include/ewebgui.hrl").
-compile(export_all).
-import(ewg_access, [get_form_param/1, get_form_params/0]).

webgui_info() -> [
    {handlers, [
        {"/groups", { %% path under the application path (which is configured in ewg_apps:store_app e.g. ewg_apps:store_app({"/admin", ewg_cb_admin}))
            groups, %% required permissions for this handler
            "Groups", %% menu text, if menu is not required for this handler then is should be set to the atom : nomenu
            {handlers, [
                {"/add", {
                    add,
                    "Add",
                    {route, [ %% route: multiple actions on the same path depending on matching input parameters
                        {
                            ["group_name", "add_group"], %% request parameters to match for this route (see ewg_apps:match_route)
                            { %% validations to perform on parameters (see ewg_lib:validate)
                                [
                                    {"group_name", mandatory, {regex,"^.{3,20}$", "Group name must be between 3 and 20 characters long"}},
                                    {"description", optional, {regex,"^.{3,20}$", "Description must be between 3 and 20 characters long"}}
                                ],
                                {ewg_admin_groups, add_group} %% callback {M, F} call M:F(ValidatedParams, ValidationErrors, RemainingPermissions, RemainingPath)
                            }
                        },
                        {
                            [], %% request parameters to match for this route [] means this is the default
                            {[], {ewg_admin_groups, add_group_form}}
                        }
                    ]}
                }},
                {"/edit", {
                    edit,
                    nomenu,
                    {route, [ %% route: multiple actions on the same path depending on matching input parameters
                        {
                            ["group_name", "edit_group", "description"], %% request parameters to match for this route (see ewg_apps:match_route)
                            { %% validations to perform on parameters (see ewg_lib:validate)
                                [
                                    {"group_name", mandatory, {regex,"^.{3,20}$", "Group name must be between 3 and 20 characters long"}},
                                    {"description", optional, {regex,"^.{3,20}$", "Description must be between 3 and 20 characters long"}}
                                ],
                                {ewg_admin_groups, add_group} %% callback {M, F} call M:F(ValidatedParams, ValidationErrors, RemainingPermissions, RemainingPath)
                            }
                        },
                        {
                            [], %% request parameters to match for this route [] means this is the default
                            {[], {ewg_admin_groups, add_group_form}}
                        }
                    ]}
                }},
                {"/list", {
                    list,
                    "List",
                    {route, [
                        {
                            [{"ewg_hidden_action", "delete"}, "ewg_hidden_item_id"],
                            {[], {ewg_admin_groups, delete_group}}
                        },
                        {
                            [{"ewg_hidden_action", "edit"}, "ewg_hidden_item_id"],
                            {[], {ewg_admin_groups, edit_group_form}}
                        },
                        {
                            ["edit_group", "group_name", "description"],
                            {
                                [
                                    {
                                        "description",
                                        optional,
                                        {
                                            regex,
                                            "^.{3,20}$",
                                            "Description must be between 3 and 20 characters long"
                                        }
                                    }
                                ],
                                {ewg_admin_groups, edit_group}}
                        },
                        {
                            [],
                            {[], {ewg_admin_groups, list_groups}} %% Default
                        }
                    ]}

                }}
            ]}
        }}
    ]},
    %% For request handling the permissions need to handle the hierarchy of handlers
    %% This is necessary since the handler info is also used to display menus based on
    %% user permissions or deny access right away to handlers the user don't have access to.
    %%
    %% This structure below is to expose the permissions to the admin GUI.
    %% Sometimes the handler hierarchy looks odd on the GUI so it is possible to
    %% define the hierarchy as a path yet have it displayed as flat.
    %% Example below is the edit, delete permissions that are checked under the
    %% list handler.
    {permissions, [
        {groups, "Groups", "Group Administration", [
            {add, "Add", "Add New Groups"},
            {list, "List", "List Groups"},
            %{[list, view_all], "View All", "View all groups"},
            {[list, edit], "Edit", "Edit group data"},
            {[list, delete], "Delete", "Delete group"}
        ]}
    ]}
].


edit_group_form(_, _) ->
    GroupName = get_form_param("ewg_hidden_item_id"),
    Description = ewg_access:get_group_var(GroupName, description),
    Permissions = ewg_access:get_group_var(GroupName, permissions),
    ewg_access:set_form_params([
        {"group_name", GroupName},
        {"description", Description}
    ] ++ [
        {"ewg_permission:" ++ P, "on"} || P <- permission_to_param(Permissions)
    ]),
    edit_group_form_int().

edit_group(validated, _) ->
    GroupName = get_form_param("group_name"),
    Description = get_form_param("description"),
    NewPermissions = parse_permissions(get_form_params(), []),
    ewg_access:set_group_var(GroupName, permissions, NewPermissions),
    ewg_access:set_group_var(GroupName, description, Description),
    ewg_access:set_group_var(GroupName, webadmin_managed_groups, all),
    {redirect, [], "list"};

edit_group(validation_failed, _) ->
    edit_group_form_int().

edit_group_form_int() ->
    Name = get_form_param("group_name"),
    {[
        {macro, {page_help, "Edit group"}},
        {h2, [], "Edit Group"}, hr,
        {macro, {eform,
            [
                {"Group Name", Name},
                {"Description", {macro, {input_text, "description"}}},
                {"Application", "Permissions"}
            ] ++ render_apps(),
            [
                {macro, {input_hidden, "group_name"}},
                {input, [
                    {type, "submit"}, {name, "edit_group"},
                    {value, "Edit Group"}
                ]}
            ]
        }},
        {form, [{action, "list"}], [{input, [{type, "submit"}, {value, "Back to Group List"}]}]}
    ],
    [
        {subhead, "Edit Web GUI user group"},
        {title, "Edit Web GUI user group"}
    ]}.

permission_to_param(Permissions) ->
    permission_to_param(Permissions, [], []).

permission_to_param([{Permission, SubPermissions} | Rest], TreeAcc, PermAcc) ->
    NewTreeAcc = concat_permission(TreeAcc, Permission),
    SubParams = permission_to_param(SubPermissions, TreeAcc, []),
    permission_to_param(
        Rest,
        TreeAcc,
        PermAcc ++
            [concat_permission(TreeAcc, Permission)]++
            [concat_permission(NewTreeAcc, SP) || SP <- SubParams]
    );
permission_to_param([Permission | Rest], TreeAcc, PermAcc) when is_atom(Permission) ->
    permission_to_param(
        Rest,
        TreeAcc,
        PermAcc ++ [concat_permission(TreeAcc, Permission)]
    );
permission_to_param([], _, PermAcc) ->
    PermAcc.

concat_permission(L, T) when is_atom(T) ->
    concat_permission(L, atom_to_list(T));
concat_permission([], T) ->
    T;
concat_permission(L, T) ->
    L ++ "|" ++ T.

add_group(validated, _Permissions) ->
    GroupName = get_form_param("group_name"),
    %% check first if the group already exists and throw back the validation
    case ewg_access:get_group_data(GroupName) of
        undefined ->
            Params = get_form_params(),
            NewPermissions = parse_permissions(Params, []),
            ewg_access:add_group(GroupName, [
                {description, get_form_param("description")},
                {permissions, NewPermissions},
                %% This we'll eventually replace with generic user/group data
                %% administration
                {webadmin_managed_groups, all}
            ]),
            redirect;
        _ ->
            ewg_access:set_validation_result("group_name", ["Group already exists !"]),
            add_group_form_int()
    end;

add_group(validation_failed, _Permissions) ->
    add_group_form_int().

add_group_form(validated, _Permissions) ->
    add_group_form_int().

add_group_form_int() ->
    {[
        {macro, {page_help, "Add new group to the webgui"}},
        {h2, [], "Add Group"}, hr,
        {macro, {eform,
            [
                {"Group Name", {macro, {input_text, "group_name"}}},
                {"Description", {macro, {input_text, "description"}}},
                {"Application", "Permissions"}
            ] ++ render_apps(),
            [
                {input, [
                    {type, "submit"}, {name, "add_group"},
                    {value, "Add Group"}
                ]}
            ]
        }}
    ],
    [
        {subhead, "Add Web GUI user group"},
        {title, "Add Web GUI user group"}
    ]}.

render_apps() ->
    [render_app(App) || App <- ewg_apps:list_apps()].

render_app({_Path, {Tag, Name, Description, Info}}) ->
    Permissions = ewg_lib:plget(permissions, Info),
    {Name, render_permissions(Permissions, [Tag]), [{hint, Description}]}.

render_permissions(Permissions, Parent) ->
    {table, [{class, "ewg_permission_table fullwidth"}],
        [render_permission(P, Parent) || P <- Permissions]
    }.

render_permission({Tag, Name, Description} , Parents) ->
    render_permission({Tag, Name, Description, []}, Parents);

render_permission({Tag, Name, Description, Sub} , Parents) ->
    PermissionPath = if
        is_list(Tag) -> Parents ++ Tag;
        true -> Parents ++ [Tag]
    end,
    {tr, [{title, Description}], [
        {td, [{width, "150px"}], Name},
        {td, [], case Sub of
            [] ->
                {macro, {
                    input_checkbox,
                    "ewg_permission:" ++ string:join([atom_to_list(P) || P <- PermissionPath], "|")
                }};
            Sub ->
                render_permissions(Sub, PermissionPath)
        end}
    ]}.

parse_permissions(Params, Permissions) ->
    lists:foldl(
        fun
            ({"ewg_permission:" ++ PermTxt, "on"}, Acc) ->
                PermissionPath = [list_to_atom(T) || T <- string:tokens(PermTxt, "|")],
                add_permission(PermissionPath, Acc);
            (_, Acc) ->
                Acc
        end,
        Permissions,
        Params
    ).

add_permission([Level], Acc) ->
    Acc ++ [Level];
add_permission([Level | Rest], Permissions) ->
    case lists:keyfind(Level, 1, Permissions) of
        false ->
            %% no tuple (yet), check if it is present as "endpoint"
            case lists:member(Level, Permissions) of
                true ->
                    lists:delete(Level, Permissions) ++ [{Level , add_permission(Rest, [])}];
                false ->
                    Permissions ++ [{Level , add_permission(Rest, [])}]
            end;
        {Level, Value} ->
            lists:keyreplace(Level, 1, Permissions, {Level, add_permission(
                Rest,
                case is_atom(Value) of true -> [Value]; _ -> Value end
            )})
    end.

delete_group(_, _) ->
    ewg_access:delete_group(get_form_param("ewg_hidden_item_id")),
    redirect.

list_groups(validated, Permissions) ->
    [DeleteP, EditP, ViewAllP] = [lists:member(Tag, Permissions) || Tag <- [delete, edit, view_all]],
    Groups = lists:sort([
        Group || Group <- ewg_access:list_groups(),
        ViewAllP orelse is_managed_group(Group)
    ]),
    {[
        {h2, [], "List GUI groups"},hr,
        {macro, {page_help, "List and manage groups"}},
        {form, [{method, "POST"}], [
            {macro, {datatable,
                ["Group", "Description", "Nr Users"] ++
                if EditP -> ["Edit"]; true -> [] end ++
                if DeleteP -> ["Delete"]; true -> [] end,
                lists:map(
                    fun(Group) ->
                        UserCount = lists:sum([
                            case ewg_access:get_user_var(User, group) of
                                Group -> 1;
                                _ -> 0
                            end || User <- ewg_access:list_users()
                        ]),
                        [
                            Group,
                            case ewg_access:get_group_var(Group, description) of
                                Desc when is_list(Desc) -> Desc;
                                _ -> ""
                            end,
                            {p, [{class, "center"}], [integer_to_list(UserCount)]}
                        ] ++
                        case {is_managed_group(Group), EditP} of
                            {true, true} ->
                                [{p, [{class, "center"}], {macro, {
                                    action_postback, Group,
                                    "edit",
                                    [{img, [{src, "/images/edit.gif"}]}]
                                }}}];
                            {false, true} -> [br];
                            _ ->
                                []
                        end ++
                        case {is_managed_group(Group), DeleteP, UserCount} of
                            {true, true, 0} ->
                                [{p, [{class, "center"}], {macro, {
                                    delete_postback, Group,
                                    "Are you sure you want to delete group : " ++ Group ++ " ?"
                                }}}];
                            {true, true, _} ->
                                [{p, [{class, "center"}], [{img, [{src, "/images/delete_gray.gif"}]}]}];
                            {false, true, _} ->
                                [br];
                            _ -> []
                        end
                    end,
                    Groups
                ),
                []
            }}
        ]}
    ],[
        {subhead, "Admin - Groups"},
        {title, "Admin - Groups"}
    ]}.

is_managed_group(Group) ->
    case ewg_access:get_group_var(webadmin_managed_groups) of
        all -> true;
        undefined -> false;
        Groups ->
            lists:member(Group, Groups)
    end.
