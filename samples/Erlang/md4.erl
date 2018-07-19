-module(md4).
-export([md4/0]).

md4() ->
    <<MD4:128>> = crypto:md4("Rosetta Code"),
    io:fwrite("Rosetta Code => ~.16B~n",[MD4]).
