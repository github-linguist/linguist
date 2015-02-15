-module(echo).
-export([start/0]).

start() ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(12321, [{packet, line}]),
                    echo_loop(Sock)
          end).

echo_loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    io:format("Got connection: ~p~n", [Conn]),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    echo_loop(Sock).

handle(Conn) ->
    receive
        {tcp, Conn, Data} ->
            gen_tcp:send(Conn, Data),
            handle(Conn);
        {tcp_closed, Conn} ->
            io:format("Connection closed: ~p~n", [Conn])
    end.
