-module(ewebgui).
-behaviour(application).
-export([start/0, start/2, stop/0, stop/1, webgui_register/0]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

start() ->
    start([], []).

start(_, _) ->
    ewebgui_i:install_check(),
    webgui_register(),
    %% Start supervisor and remember its pid to make app controller happy
    {ok, SupPid} = ewg_sup:start_link(),
    %% Prepare yaws to start embedded
    LibDir = code:lib_dir(ewebgui),
    VMRoot = filename:dirname(filename:dirname(LibDir)) ++ "/",
    DocRoot = LibDir ++ "/priv/www/",
    LogDir = ewg_conf:read(logdir, VMRoot ++ "webgui_logs"),
    file:make_dir(LogDir),
    Id = "ewebgui",
    GConf = [
        {logdir, LogDir},
        {id, Id}
    ],
    SConf = [
        %% Keep the html root in the priv directory of this app
        {docroot, DocRoot},
        %% This is quite nice: can add extra docroots so apps can include their
        %% own static css/js/image items. Tested and works. Will make something
        %% of it later.
        %% {xtra_docroots, [VMRoot ++ "/test_docroot/"]},
        {port, ewg_conf:read(port, 8443)},
        {listen, ewg_conf:read(listen_address, {0,0,0,0})},
        %% use root appmod, really the only way to centralize access control
        {appmods, [{"/", ewg_appmod, [["js"], ["css"], ["images"]]}]}
    ] ++
    case ewg_conf:read(use_ssl, true) of
        true ->
            CertDir = ewg_conf:read(cert_dir, LibDir ++ "/priv/cert"),
            [{ssl, #ssl{
                keyfile = CertDir ++ "/key.pem",
                certfile = CertDir ++ "/cert.pem",
                cacertfile = CertDir ++ "/cacerts.pem"
            }}];
        _ ->
            []
    end,
    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(DocRoot, SConf, GConf, Id),
    [supervisor:start_child(ewg_sup, Child) || Child <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, SupPid}.

stop() ->
    stop([]).

stop(_) ->
    ok.

%% register the app
webgui_register() ->
    ewg_apps:store_app("/webgui_admin", {webgui_admin, "GUI Admin", "Webgui administration", [
        ewg_admin_user_log:webgui_info(),
        ewg_admin_users:webgui_info(),
        ewg_admin_groups:webgui_info()
    ]}).
