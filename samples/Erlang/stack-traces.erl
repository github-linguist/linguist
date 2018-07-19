-module(stack_traces).

-export([main/0]).

main() ->
	{ok,A} = outer(),
	io:format("~p\n", [A]).

outer() ->
	{ok,A} = middle(),
	{ok,A}.

middle() ->
	{ok,A} = inner(),
	{ok,A}.

inner() ->
	try throw(42) catch 42 -> {ok,erlang:get_stacktrace()} end.
