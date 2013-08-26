-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-include("../include/vis_request.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->	
    lager:notice("websocket init"),
    lager:info("websocket init", [Req]),
    gproc:reg({p,l, main_room}),
    {ok, Req, 0}.

websocket_handle(_Data, Req, State) ->	
    lager:notice("websocket handle"),
    lager:info("websocket handle", [Req]),
    {ok, Req, State}.

websocket_info({_Pid, ?WSBroadcast, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(Info, Req, State) ->
    lager:info("not response, req - ~p, info - ~p", [Req, Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, Req, _State) ->	
    lager:notice("websocket terminate"),
    lager:info("websocket terminate", [Req]),
    ok.
