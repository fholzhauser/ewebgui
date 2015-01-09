-module(ewg_status).
-export([show/0, show/1]).

%% Here we'll get application status things (widget if you like) where
%% applications can put their "things" that will be rendered on all pages.
%% e.g. quick status indications like alarms
show() ->
    show("default").

show(_Group) ->
    case ewg_conf:read(status_box_callback) of
        {M, F} ->
            case catch M:F() of
                Statuses when is_list(Statuses) ->
                    [{'div', [{class, "ewg_conn_status"}],[
                        {p, [], [
                            {img, [{src, if
                                S == red ->
                                    "/images/red.png";
                                S == green ->
                                    "/images/green.png";
                                S == yellow ->
                                    "/images/yellow.png";
                                true ->
                                    "/images/grey.png"
                            end}]},
                            Text
                        ]}
                    || {Text, S} <- Statuses]}];
                _ ->
                    []
            end;
        _ ->
            []
    end.
