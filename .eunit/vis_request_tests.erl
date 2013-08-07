-module(vis_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/vis_request.hrl").

-define(TRACKER_URL, "http://localhost:8080").

vis_request_test_() -> 
  {spawn,
    {setup,
      fun () -> start_apps() end, % init
      fun (_) -> stop_apps() end, % clean
      [fun test_push_ip/0]
    }
  }.

test_push_ip() ->  
  [?_assertEqual(ok, element(1, vis_request:push_ip("109.195.193.137"))),
   ?_assertError(einval, vis_request:push_ip("test")),
   ?_assertError(badmatch, vis_request:push_ip(<<123>>))].

start_apps() ->
    vis_request:start().

stop_apps() ->
    vis_request:stop().
