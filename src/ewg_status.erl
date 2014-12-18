-module(ewg_status).
-export([show/0, show/1]).

%% Here we'll get application status things (widget if you like) where
%% applications can put their "things" that will be rendered on all pages.
%% e.g. quick status indications like alarms
show() ->
    show("default").

show(_Group) ->
[].
