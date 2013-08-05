-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(WSBroadcast,"wsbroadcast").

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->	
	lager:notice("websocket init"),
	lager:info("websocket init", [Req]),
	self()!<<"Hello!">>,
	gproc:reg({p,l, main_room}),
	{ok, Req, undefined_state}.

websocket_handle(_Data, Req, State) ->	
	lager:notice("websocket handle"),
	lager:info("websocket handle", [Req]),
	{ok, Req, State}.

websocket_info({Pid, ?WSBroadcast, Msg}, Req, State) ->	
	lager:notice("websocket info"),
	lager:info("websocket info", [Req]),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->	
	lager:notice("websocket info 2"),
	lager:info("websocket info 2", [Req]),
	{ok, Req, State}.

websocket_terminate(_Reason, Req, _State) ->	
	lager:notice("websocket terminate"),
	lager:info("websocket terminate", [Req]),
	ok.
