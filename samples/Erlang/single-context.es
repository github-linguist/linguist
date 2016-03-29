#! /usr/bin/env escript
% This file is part of Emonk released under the MIT license. 
% See the LICENSE file for more information.

main([]) ->
	start(64, 1000);
main([N]) ->
	start(list_to_integer(N), 1000);
main([N, M]) ->
	start(list_to_integer(N), list_to_integer(M)).


start(N, M) ->
	code:add_pathz("test"),
	code:add_pathz("ebin"),
	{ok, Ctx} = emonk:create_ctx(),
	{ok, undefined} = emonk:eval(Ctx, js()),
	run(Ctx, N, M),
	wait(N).

run(_, 0, _) ->
	ok;
run(Ctx, N, M) ->
	Self = self(),
	Pid = spawn(fun() -> do_js(Self, Ctx, M) end),
	io:format("Spawned: ~p~n", [Pid]),
	run(Ctx, N-1, M).

wait(0) ->
	ok;
wait(N) ->
	receive
		{finished, Pid} -> ok
	end,
	io:format("Finished: ~p~n", [Pid]),
	wait(N-1).

do_js(Parent, _, 0) ->
	Parent ! {finished, self()};
do_js(Parent, Ctx, M) ->
	io:format("Running: ~p~n", [M]),
	Test = random_test(),
	{ok, [Resp]} = emonk:call(Ctx, <<"f">>, [Test]),
	Sorted = sort(Resp),
	true = Test == Sorted,
	do_js(Parent, Ctx, M-1).

js() -> 
	<<"var f = function(x) {return [x];};">>.

random_test() ->
	Tests = [
		null,
		true,
		false,
		1,
		-1,
		3.1416,
		-3.1416,
		12.0e10,
		1.234E+10,
		-1.234E-10,
		10.0,
		123.456,
		10.0,
		<<"foo">>,
		<<"foo", 5, "bar">>,
		<<"">>,
		<<"\n\n\n">>,
		<<"\" \b\f\r\n\t\"">>,
		{[]},
		{[{<<"foo">>, <<"bar">>}]},
		{[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]},
		[],
		[[]],
		[1, <<"foo">>],
		{[{<<"foo">>, [123]}]},
		{[{<<"foo">>, [1, 2, 3]}]},
		{[{<<"foo">>, {[{<<"bar">>, true}]}}]},
		{[
			{<<"foo">>, []},
			{<<"bar">>, {[{<<"baz">>, true}]}}, {<<"alice">>, <<"bob">>}
		]},
		[-123, <<"foo">>, {[{<<"bar">>, []}]}, null]
	],
	{_, [Test | _]} = lists:split(random:uniform(length(Tests)) - 1, Tests),
	sort(Test).

sort({Props}) ->
	objsort(Props, []);
sort(List) when is_list(List) ->
	lstsort(List, []);
sort(Other) ->
	Other.

objsort([], Acc) ->
	{lists:sort(Acc)};
objsort([{K,V} | Rest], Acc) ->
	objsort(Rest, [{K, sort(V)} | Acc]).

lstsort([], Acc) ->
	lists:reverse(Acc);
lstsort([Val | Rest], Acc) ->
	lstsort(Rest, [sort(Val) | Acc]).
