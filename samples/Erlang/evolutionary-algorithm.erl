-module(evolution).
-export([run/0]).

-define(MUTATE, 0.05).
-define(POPULATION, 100).
-define(TARGET, "METHINKS IT IS LIKE A WEASEL").
-define(MAX_GENERATIONS, 1000).

run() -> evolve_gens().

evolve_gens() ->
    Initial = random_string(length(?TARGET)),
    evolve_gens(Initial,0,fitness(Initial)).
evolve_gens(Parent,Generation,0) ->
    io:format("Generation[~w]: Achieved the target: ~s~n",[Generation,Parent]);
evolve_gens(Parent,Generation,_Fitness) when Generation == ?MAX_GENERATIONS ->
    io:format("Reached Max Generations~nFinal string is ~s~n",[Parent]);
evolve_gens(Parent,Generation,Fitness) ->
    io:format("Generation[~w]: ~s, Fitness: ~w~n",
              [Generation,Parent,Fitness]),
    Child = evolve_string(Parent),
    evolve_gens(Child,Generation+1,fitness(Child)).

fitness(String) -> fitness(String, ?TARGET).
fitness([],[]) -> 0;
fitness([H|Rest],[H|Target]) -> fitness(Rest,Target);
fitness([_H|Rest],[_T|Target]) -> 1+fitness(Rest,Target).

mutate(String) -> mutate(String,[]).
mutate([],Acc) -> lists:reverse(Acc);
mutate([H|T],Acc) ->
    case random:uniform() < ?MUTATE of
        true ->
            mutate(T,[random_character()|Acc]);
        false ->
            mutate(T,[H|Acc])
    end.

evolve_string(String) ->
    evolve_string(String,?TARGET,?POPULATION,String).
evolve_string(_,_,0,Child) -> Child;
evolve_string(Parent,Target,Population,Best_Child) ->
    Child = mutate(Parent),
    case fitness(Child) < fitness(Best_Child) of
        true ->
            evolve_string(Parent,Target,Population-1,Child);
        false ->
            evolve_string(Parent,Target,Population-1,Best_Child)
    end.

random_character() ->
    case random:uniform(27)-1 of
        26  -> $ ;
        R -> $A+R
    end.

random_string(Length) -> random_string(Length,[]).
random_string(0,Acc) -> Acc;
random_string(N,Acc) when N > 0 ->
    random_string(N-1,[random_character()|Acc]).
