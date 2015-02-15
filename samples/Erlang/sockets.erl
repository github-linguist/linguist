-module(socket).
-export([start/0]).

start() ->
    {ok, Sock} = gen_tcp:connect("localhost", 256,
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "hello socket world"),
    ok = gen_tcp:close(Sock).
