-module(tok).
-export([start/0]).

start() ->
   Lst = string:tokens("Hello,How,Are,You,Today",","),
   io:fwrite("~s~n", [string:join(Lst,".")]),
   ok.
