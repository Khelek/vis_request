-module(vis_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/vis_request.hrl").

-export([tracker_url/0]).

-record(websocket_params,  {responseCount = 100000, timeout = 0}).
-record(params, {websocket_params = #websocket_params{} }).

vis_request_test_() ->
    {spawn,
     {setup,
      fun () -> start_apps() end, % init
      fun (_) -> stop_apps() end, % clean
      fun(Params) ->
              [test_push_ip(),
               test_broadcast(),
               test_egeoip(),
               test_connect(),
               test_websocket_connect(Params)]
      end
     }
    }.

test_push_ip() ->  
    [?_assertEqual(ok, element(1, vis_request:push_ip("109.195.193.137")))].

test_broadcast() ->
    [?_assertNotException(error, badmatch, 
                          {_Pid, "wsbroadcast", <<"[54,48]">>} = vis_request_app:vis_request_broadcast("109.195.193.137")
                         )].

test_connect() -> 
    ?_assertEqual(ok, element(1, (fun() -> {ok, Client} = cowboy_client:init([]),
                                           cowboy_client:request(<<"GET">>, tracker_url(), Client)
                                  end)()
                             )).

test_websocket_connect(Params) ->
    {timeout,
     websocket_params(timeout, Params),
     [?_assert((fun() -> {ok, Port} = application:get_env(vis_request, port),
                         {ok, Host} = application:get_env(vis_request, host),
                         w_client:websocket_client(Host, Port, "/websocket", [{active, false}]),
                         {Pid, _} = lists:last(gproc:lookup_local_properties(main_room)),
                         vis_request:bench(websocket_params(responseCount, Params)),
                         case erlang:process_info(Pid, message_queue_len) of
                             {message_queue_len, Count} when Count > 500 -> false;
                             _ -> true
                         end
                end)())]
    }.

test_egeoip() -> 
    ?_assertEqual(ok, element(1, egeoip:lookup("109.195.193.137"))).

start_apps() -> vis_request:start(), test_params().

stop_apps() -> ok.

%% utilite
tracker_url() -> 
    {ok, Port} = application:get_env(vis_request, port),
    {ok, Host} = application:get_env(vis_request, host),
    Url = lists:concat(["http://", Host, ":", Port]),
    list_to_binary(Url).

websocket_params(timeout, Params) ->
    Params#params.websocket_params#websocket_params.timeout;
websocket_params(responseCount, Params) ->
    Params#params.websocket_params#websocket_params.responseCount.

test_params() ->
    {ok,[{ws_q,[{hz, Hz},
                {rate, Rate},
                {token_limit, _Tokens},
                {size, _Size},
                {concurrency, _Conc},
                {queue_type, _Type}]} | _Other]} = application:get_env(safetyvalve, queues),
    ResponsesCount = #websocket_params{}#websocket_params.responseCount,
    Timeout = (ResponsesCount / (Rate / (Hz / 1000))) * 1.1,
    _Params = #params{websocket_params = #websocket_params{timeout = Timeout}}.
