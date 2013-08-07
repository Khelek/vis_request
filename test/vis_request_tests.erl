-module(vis_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/vis_request.hrl").

-define(TRACKER_URL, "http://localhost:8080").

vis_request_test_() -> 
  {spawn,
    {setup,
      fun () -> start_apps() end, % init
      fun (_) -> stop_apps() end, % clean
      [test_push_ip(),
       test_broadcast(),
       test_egeoip()]
    }
  }.

test_push_ip() ->  
  [?_assertEqual(ok, element(1, vis_request:push_ip("109.195.193.137"))),
   ?_assertError({badmatch,{error,einval}}, vis_request:push_ip("test")),
   ?_assertError({badmatch,{error,badmatch}}, vis_request:push_ip(<<123>>))].

test_broadcast() ->
  [?_assertNotException(error, badmatch, 
    {_Pid, "wsbroadcast", <<"[54,48]">>} = vis_request_app:vis_request_broadcast("109.195.193.137"))].

test_connect() -> 
  {spawn,
    {setup,
      fun () -> ok end, % init
      fun (_) -> stop_apps() end, % clean
      [?assertNotException(error, badmatch, 
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = 
           httpc:request(?TRACKER_URL))]
    }
  }.

test_egeoip() -> 
  ?_assertEqual(ok, element(1, egeoip:lookup("109.195.193.137"))).

start_apps() ->
    vis_request:start().

stop_apps() -> ok.
    %vis_request:stop().
