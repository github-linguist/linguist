-module( josephus_problem ).

-export( [general_solution/3, task/0] ).

general_solution( Prisoners, Kill, Survive ) -> general_solution( Prisoners, Kill, Survive, erlang:length(Prisoners), [] ).

task() -> general_solution( lists:seq(0, 40), 3, 1 ).



general_solution( Prisoners, _Kill, Survive, Survive, Kills ) ->
        {Prisoners, lists:reverse(Kills)};
general_solution( Prisoners, Kill, Survive, Prisoners_length, Kills ) ->
        {Skipped, [Killed | Rest]} = kill( Kill, Prisoners, Prisoners_length ),
        general_solution( Rest ++ Skipped, Kill, Survive, Prisoners_length - 1, [Killed | Kills] ).

kill( Kill, Prisoners, Prisoners_length ) when Kill < Prisoners_length ->
    lists:split( Kill - 1, Prisoners );
kill( Kill, Prisoners, Prisoners_length ) ->
    kill_few( Kill rem Prisoners_length, Prisoners ).

kill_few( 0, Prisoners ) ->
    [Last | Rest] = lists:reverse( Prisoners ),
    {lists:reverse( Rest ), [Last]};
kill_few( Kill, Prisoners ) ->
    lists:split( Kill - 1, Prisoners ).
