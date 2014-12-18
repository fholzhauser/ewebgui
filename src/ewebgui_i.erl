-module(ewebgui_i).

-export([install_check/0, install/1]).

install(Nodes) ->
    [
        {T, mnesia:create_table(T, Opt)} || {T, Opt} <- [
            {D, [{disc_copies, Nodes}]} || D <- [
                ewg_user,
                ewg_group,
                ewg_conf
            ]
        ]
    ].

%% This will check if mnesia already has a schema, if yes then it installs there.
%% If not (blank start) then it installs a new single node schema.

install_check() ->
    Nodes = install_schema_if_needed(),
    ok = mnesia:start(),
    install_tables_if_needed(Nodes),
    wait_for_all_tables(),
    add_superuser_if_needed().

install_tables_if_needed(Nodes) ->
    [
        create_table_if_needed(TableDef) || TableDef <- [
            {ewg_user, [{disc_copies, Nodes}]},
            {ewg_group, [{disc_copies, Nodes}]},
            {ewg_app, [{disc_copies, Nodes}]},
            {ewg_conf, [{disc_copies, Nodes}]}
        ]
    ].

% check schema and create if not there yet
install_schema_if_needed() ->
    % mnesia is not supposed to be running yet
    mnesia:start(),
    % default installation is single node. perhaps later we'll read some install
    % config here
    case mnesia:table_info(schema, disc_copies) of
        [] ->
            % no schema yet
            mnesia:stop(),
            ok = mnesia:create_schema([node()]),
            [node()];
        _ ->
            % there is some schema, pretend it is ok
            mnesia:system_info(db_nodes)
    end.

create_table_if_needed({Table, Opts}) ->
    {Table, case lists:member(Table, mnesia:system_info(tables)) of
        true ->
            existing;
        false ->
            {atomic, ok} = mnesia:create_table(Table, Opts),
            created
    end}.


wait_for_all_tables() ->
    Tables = mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables, 60000).

add_superuser_if_needed() ->
    case ewg_access:list_users() of
        [] ->
            ewg_access:add_group("install_admin", [
                {permissions, [
                    {webgui_admin, [
                        change_own_pw, audit_log,
                        {users, [
                            add,
                            {list, [view_all,edit,reset_pw,delete]}
                        ]},
                        {groups, [
                            add,
                            {list, [view_all, edit, delete]}
                        ]}
                    ]}
                ]},
                {webadmin_managed_groups, all}
            ]),
            ewg_access:add_user("install", "install", [{group, "install_admin"}]);
        _ ->
            ok
    end.

