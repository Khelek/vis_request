-module(vis_request_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/vis_request.hrl").

-define(TRACKER_URL, "http://localhost:8080").

vis_request_test_() ->
    .


start_apps() ->
    vis_request:start().

stop(_SD) ->
    vis_request:stop().

cleaner_test_start() ->
    application:load(etracker),
    application:set_env(etracker, clean_interval, 5),
    etracker:start(),
    timer:sleep(1000),
    cleaner_test_start1().
