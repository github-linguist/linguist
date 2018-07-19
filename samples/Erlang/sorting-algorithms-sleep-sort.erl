#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname sleepsort

main(Args) ->
    lists:foreach(fun(Arg) ->
                          timer:send_after(5 * list_to_integer(Arg), self(), Arg)
                  end, Args),
    loop(length(Args)).

loop(0) ->
    ok;
loop(N) ->
    receive
        Num ->
            io:format("~s~n", [Num]),
            loop(N - 1)
    end.
