-module(vis_request_app).
-behaviour(application).

-include("../include/vis_request.hrl").
%% API.
-export([start/2]).
-export([stop/1]).
-export([vis_request_broadcast/1]).


%% API.
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", toppage_handler, []},
                                             {"/websocket", ws_handler, []},
                                             {"/static/[...]", cowboy_static,
                                              [{directory, {priv_dir, vis_request, [<<"static">>]}},
                                               {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}]}
                                     ]),
    {ok, Port} = application:get_env(port),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}, {timeout, 20000}]).

stop(_State) -> ok.

vis_request_broadcast(Ip) ->
    Info = egeoip:lookup(Ip),
    Coords = get_coords(Info),
    gproc:send({p, l, main_room}, {self(), ?WSBroadcast, list_to_binary(Coords)}),
    kill_slow_clients(main_room).
%% utilite
kill_slow_clients(Property) ->
    ListPid = gproc:lookup_local_properties(Property),
    lists:map(fun({Pid, _}) ->
                      case erlang:process_info(Pid, message_queue_len) of
                          {message_queue_len, Count} when Count > 500 ->
                              lager:warning("killed slow client, pid ~p", [Pid]),
                              exit(Pid, kill);
                          undefined -> true;
                          _ -> true
                      end
              end, ListPid).

get_coords(Info) ->	
    {ok, {_,_,_,_,_,_,_, Lat_float, Long_float,_,_}} = Info,
    Latitude = round(Lat_float),
    Longitude = round(Long_float),
    lists:concat(["[",Latitude,",",Longitude,"]"]).



