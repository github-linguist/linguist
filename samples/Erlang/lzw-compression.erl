-module(lzw).

-export([test/0, encode/1, decode/1]).

-import(lists, [reverse/1, reverse/2]).

test() ->
    Str = "TOBEORNOTTOBEORTOBEORNOT",
    [84,79,66,69,79,82,78,79,84,256,258,260,265,259,261,263] =
	encode(Str),
    Str = decode(encode(Str)),
    ok.

encode(Str) ->
    D = init(dict:new()),
    encode(Str, D, 256, []).

encode([H], D, _, Out) ->
    Val = dict:fetch([H], D),
    reverse([Val|Out]);
encode([H|T], D, Free, Out) ->
    Val = dict:fetch([H], D),
    find_match(T, [H], Val, D, Free, Out).

find_match([H|T], L, LastVal, D, Free, Out) ->
    case dict:find([H|L], D) of
	{ok, Val} ->
	    find_match(T, [H|L], Val, D, Free, Out);
	error ->
	    D1 = dict:store([H|L], Free, D),
	    encode([H|T], D1, Free+1, [LastVal|Out])
    end;
find_match([], _, LastVal, _, _, Out) ->
    reverse([LastVal|Out]).

decode([H|T]) ->
    D   = init1(dict:new()),
    Val = dict:fetch(H, D),
    decode(T, Val, 256, D, Val).

decode([], _, _, _, L) ->
    reverse(L);
decode([H|T], Old, Free, D, L) ->
    Val = dict:fetch(H, D),
    Add = [lists:last(Val)|Old],
    D1  = dict:store(Free, Add, D),
    decode(T, Val, Free+1, D1, Val ++ L).

init(D) -> init(255, D).

init(0, D) ->  D;
init(N, D) ->  D1 = dict:store([N],N,D),  init(N-1, D1).

init1(D) -> init1(255, D).

init1(0, D) ->  D;
init1(N, D) ->  D1 = dict:store(N,[N],D),  init1(N-1, D1).
