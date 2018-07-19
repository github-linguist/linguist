-module(beersong).
-export([sing/0]).
-define(TEMPLATE_0, "~s of beer on the wall, ~s of beer.~nGo to the store and buy some more, 99
bottles of beer on the wall.~n").
-define(TEMPLATE_N, "~s of beer on the wall, ~s of beer.~nTake one down and pass it around, ~s of
beer on the wall.~n~n").

create_verse(0)      -> {0, io_lib:format(?TEMPLATE_0, phrase(0))};
create_verse(Bottle) -> {Bottle, io_lib:format(?TEMPLATE_N, phrase(Bottle))}.

phrase(0)      -> ["No more bottles", "no more bottles"];
phrase(1)      -> ["1 bottle", "1 bottle", "no more bottles"];
phrase(2)      -> ["2 bottles", "2 bottles", "1 bottle"];
phrase(Bottle) -> lists:duplicate(2, integer_to_list(Bottle) ++ " bottles") ++
[integer_to_list(Bottle-1) ++ " bottles"].

bottles() -> lists:reverse(lists:seq(0,99)).

sing() ->
    lists:foreach(fun spawn_singer/1, bottles()),
    sing_verse(99).

spawn_singer(Bottle) ->
    Pid = self(),
    spawn(fun() -> Pid ! create_verse(Bottle) end).

sing_verse(Bottle) ->
    receive
        {_, Verse} when Bottle == 0 ->
            io:format(Verse);
        {N, Verse} when Bottle == N ->
            io:format(Verse),
            sing_verse(Bottle-1)
    after
        3000 ->
            io:format("Verse not received - re-starting singer~n"),
            spawn_singer(Bottle),
            sing_verse(Bottle)
    end.
