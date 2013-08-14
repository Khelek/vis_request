-compile(export_all).
-module(vis_request_client).

%% API.
-export([my_websocket_client/0]).


%% API.
my_websocket_client() ->
	websocket_client("localhost", 8081, "/websocket").

websocket_client(Host, Port, Path) -> 
	Socket = connect(Host, Port),
	Key = generate_ws_key(),
	ok = websocket_handshake(Socket, Host, Path, Key),
	Socket. 

my_client() ->
	Request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n",
	client("localhost", 8081, Request).

client(Host, Port, Request) -> 
	Socket = connect(Host, Port),
	Response = send_and_receive(Socket, Request),	
	gen_tcp:close(Socket),
    Response.

send_and_receive(Socket, Request) -> 
	gen_tcp:send(Socket, Request),
	gen_tcp:recv(Socket, 0).

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
                  crypto:sha(<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
    {match, [Challenge]} = re:run(
                             HandshakeResponse,
                             ".*[s|S]ec-[w|W]eb[s|S]ocket-[a|A]ccept: (.*)\\r\\n.*",
                             [{capture, [1], binary}]),
    ok.

generate_ws_key() ->
    base64:encode(crypto:rand_bytes(16)).





