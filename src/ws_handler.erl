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
	{ok, Req, undefined_state}.

websocket_handle(_Data, Req, State) ->	
	lager:notice("websocket handle"),
	lager:info("websocket handle", [Req]),
	{ok, Req, State}.

websocket_info({Pid, ?WSBroadcast, Msg}, Req, State) ->
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, Count} when Count > 500 ->  lager:warning("too many messages in message queue ~p", [Count]),
    		{shutdown, Req, State};
        undefined -> lager:error("undefined erlang:process_info"), {shutdown, Req, State};
        _ -> {reply, {text, Msg}, Req, State}
    end;
	
websocket_info(_Info, Req, State) ->	
	lager:notice("not response"),
	lager:info("not response", [Req]),
	{ok, Req, State}.

websocket_terminate(_Reason, Req, _State) ->	
	lager:notice("websocket terminate"),
	lager:info("websocket terminate", [Req]),
	ok.
