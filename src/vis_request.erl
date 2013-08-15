%% Feel free to use, reuse and abuse the code in this file.

-module(vis_request).

%% API.
-export([start/0, stop/0, push_ip/1, bench/1]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(gproc),
    ok = application:start(lager),
    ok = application:start(safetyvalve),
    ok = egeoip_just_start(),
    ok = application:start(vis_request).

stop() ->
    application:stop(vis_request),
    application:stop(safetyvalve),
    application:stop(egeoip),
    application:stop(lager),
    application:stop(gproc),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto).

-spec push_ip(string()) -> {ok, pid()}.
push_ip(Ip) ->
    sv:run(ws_q, fun() ->
       spawn(  fun() -> vis_request_app:vis_request_broadcast(Ip) end ) end).

bench(Count) ->
    SampleIPs = ["63.224.214.117",
                 "144.139.80.91",
                 "88.233.53.82",
                 "85.250.32.5",
                 "220.189.211.182",
                 "211.112.118.99",
                 "84.94.205.244",
                 "61.16.226.206",
                 "64.180.1.78",
                 "138.217.4.11"],
    StartParse = now(),
    benchcall(fun () -> [push_ip(X) || X <- SampleIPs] end, round(Count/length(SampleIPs))),
    EndParse = now(),
    {end_benchmark, unixtime(EndParse) - unixtime(StartParse)}.

%% useful utilite function

benchcall(Fun, 1) ->
    Fun();
benchcall(Fun, Times) ->
    Fun(),
    benchcall(Fun, Times - 1).

unixtime({MegaSecs, Secs, MicroSecs}) ->
    (1.0e+6 * MegaSecs) + Secs + (1.0e-6 * MicroSecs).

geodbname() ->
    {ok, Cwd} = file:get_cwd(),
    _Filename = filename:join([Cwd, "priv", "GeoLiteCity.dat"]).

egeoip_just_start() -> 
    case egeoip:start(geodbname()) 
    of ok -> ok;
       {error, {already_started,egeoip}} -> ok;
       {error, Reason} -> {error, Reason}
    end.
