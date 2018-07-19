% Implemented by Arjun Sunel
-module(read_files).
-export([main/0]).

main() ->
	Read = fun (Filename) -> {ok, Data} = file:read_file(Filename), Data end,
	Lines = string:tokens(binary_to_list(Read("read_files.erl")), "\n"),
	lists:foreach(fun (Y) -> io:format("~s~n", [Y]) end, lists:zipwith(fun(X,_)->X end, Lines, lists:seq(1, length(Lines)))).
