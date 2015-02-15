-module(aplusb).
-export([start/0]).

start() ->
    case io:fread("","~d~d") of
        eof -> ok;
        {ok, [A,B]} ->
            io:format("~w~n",[A+B]),
            start()
    end.
