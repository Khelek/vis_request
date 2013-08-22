-module(vis_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/vis_request.hrl").

-export([tracker_url/0]).

vis_request_test_() -> 
    {timeout, 900, [{spawn,
                          {setup,
                           fun () -> start_apps() end, % init
                           fun (_) -> stop_apps() end, % clean
                           [test_push_ip(),
                            test_broadcast(),
                            test_egeoip(),
                            test_connect(),
                            test_websocket_connect()]
                          }
                         }]}.

test_push_ip() ->  
    [?_assertEqual(ok, element(1, vis_request:push_ip("109.195.193.137")))].

test_broadcast() ->
    [?_assertNotException(error, badmatch, 
                          {_Pid, "wsbroadcast", <<"[54,48]">>} = vis_request_app:vis_request_broadcast("109.195.193.137")
                         )
    ].

test_connect() -> 
  ?_assertEqual(ok, element(1, (fun() -> 
                                        {ok, Client} = cowboy_client:init([]),
                                        cowboy_client:request(<<"GET">>, tracker_url(), Client) 
                                end)()
                           )
               ).

test_websocket_connect() ->
    {timeout, 900, [?_assert((fun() ->
                                        {ok, Port} = application:get_env(vis_request, port),
                                        {ok, Host} = application:get_env(vis_request, host),
                                        w_client:websocket_client(Host, Port, "/websocket", [{active, false}]),
                                        {Pid, _} = lists:last(gproc:lookup_local_properties(main_room)),    
                                        vis_request:bench(800000),
                                        case erlang:process_info(Pid, message_queue_len) of
                                            {message_queue_len, Count} -> 
                                                if 
                                                    Count > 500 -> false;
                                                    true -> true
                                                end;
                                            undefined -> true
                                        end
                                end)()
                              )]}.

test_egeoip() -> 
    ?_assertEqual(ok, element(1, egeoip:lookup("109.195.193.137"))).

start_apps() ->
    vis_request:start().

stop_apps() -> vis_request:stop().

tracker_url() -> 
    {ok, Port} = application:get_env(vis_request, port),
    {ok, Host} = application:get_env(vis_request, host),
    Url = lists:concat(["http://", Host, ":", Port]),
    list_to_binary(Url).


