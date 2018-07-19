-module(rps).
-compile(export_all).

play() ->
    loop([1,1,1]).

loop([R,P,S]=Odds) ->
    case io:fread("What is your move? (R,P,S,Q) ","~c") of
        {ok,["Q"]} -> io:fwrite("Good bye!~n");
        {ok,[[Human]]} when Human == $R; Human == $P; Human == $S ->
            io:fwrite("Your move is ~s.~n",
                      [play_to_string(Human)]),
            Computer = select_play(Odds),
            io:fwrite("My move is ~s~n",
                      [play_to_string(Computer)]),
            case {beats(Human,Computer),beats(Computer,Human)} of
                {true,_} -> io:fwrite("You win!~n");
                {_,true} -> io:fwrite("I win!~n");
                _ -> io:fwrite("Draw~n")
            end,
            case Human of
                $R -> loop([R+1,P,S]);
                $P -> loop([R,P+1,S]);
                $S -> loop([R,P,S+1])
            end;
        _ ->
            io:fwrite("Invalid play~n"),
            loop(Odds)
    end.

beats($R,$S) -> true;
beats($P,$R) -> true;
beats($S,$P) -> true;
beats(_,_) -> false.

play_to_string($R) -> "Rock";
play_to_string($P) -> "Paper";
play_to_string($S) -> "Scissors".

select_play([R,P,S]) ->
    N = random:uniform(R+P+S),
    if
        N =< R -> $P;
        N =< R+P -> $S;
        true -> $R
    end.
