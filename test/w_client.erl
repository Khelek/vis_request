-module(w_client).

%% API.
-export([deaf/0, good/0, websocket_client/4]).


%% API.

deaf() ->
	websocket_client("localhost", 8081, "/websocket", [{active, false}]).

good() -> 
  websocket_client("localhost", 8081, "/websocket", [{active, true}]).

websocket_client(Host, Port, Path, Opts) -> 
	Socket = connect(Host, Port),
  inet:setopts(Socket, [{active, false}]), % from correct websocket_handshake
	Key = generate_ws_key(),
	ok = websocket_handshake(Socket, Host, Path, Key),
  inet:setopts(Socket, Opts),
	Socket.

connect(Host, Port) -> connect(Host, Port, [binary, {packet, 0}, {active, false}, {keepalive, true}]).

connect(Host, Port, Options) -> 
	case gen_tcp:connect(Host, Port, Options) of 
		{ok, Socket} -> Socket;
		{error, _} -> throw(connection_error)
	end.

%% utilite

websocket_handshake(Socket, Host, Path, Key) ->
    Handshake = [<<"GET ">>, Path,
                 <<" HTTP/1.1"
                   "\r\nHost: ">>, Host,
                 <<"\r\nUpgrade: WebSocket"
                   "\r\nConnection: Upgrade"
                   "\r\nSec-WebSocket-Key: ">>, Key,
                 <<"\r\nOrigin: ws://">>, Host,
                 <<"\r\nSec-WebSocket-Protocol: "
                   "\r\nSec-WebSocket-Version: 13"
                   "\r\n\r\n">>],
    gen_tcp:send(Socket, Handshake),
    {ok, HandshakeResponse} = receive_handshake(<<>>, Socket),
    validate_handshake(HandshakeResponse, Key),
    ok.

receive_handshake(Buffer, Socket) ->
    case re:run(Buffer, ".*\\r\\n\\r\\n") of
        {match, _} ->
            {ok, Buffer};
        _ ->
            {ok, Data} = gen_tcp:recv(Socket, 0, 6000),
            receive_handshake(<< Buffer/binary, Data/binary >>,
                              Socket)
    end.

validate_handshake(HandshakeResponse, Key) ->
    Challenge = base64:encode(
                  crypto:hash(sha, << Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
    {match, [Challenge]} = re:run(
                             HandshakeResponse,
                             ".*[s|S]ec-[w|W]eb[s|S]ocket-[a|A]ccept: (.*)\\r\\n.*",
                             [{capture, [1], binary}]),
    ok.

generate_ws_key() ->
    base64:encode(crypto:rand_bytes(16)).





