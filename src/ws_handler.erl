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
	erlang:display("-------------------------webInit"),
	erlang:start_timer(1000, self(), <<"Hello!">>),
	gproc:reg({p,l, main_room}),
	{ok, Req, undefined_state}.

websocket_handle(_Data, Req, State) ->
	erlang:display("-------------------------webHandle2"),
	{ok, Req, State}.

websocket_info({Pid, ?WSBroadcast, Msg}, Req, State) ->
	%erlang:display("-------------------------webInfo1"),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	erlang:display("-------------------------webInfo2"),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	erlang:display("-------------------------webTerminate"),
	ok.
