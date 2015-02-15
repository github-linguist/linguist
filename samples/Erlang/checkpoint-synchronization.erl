-module( checkpoint_synchronization ).

-export( [task/0] ).

task() ->
      Pid = erlang:spawn( fun() -> checkpoint_loop([], []) end ),
      [erlang:spawn(fun() -> random:seed(X, 1, 0), worker_loop(X, 3, Pid) end) || X <- lists:seq(1, 5)],
      erlang:exit( Pid, normal ).



checkpoint_loop( Assemblings, Completes ) ->
        receive
        {starting, Worker} -> checkpoint_loop( [Worker | Assemblings], Completes );
        {done, Worker} ->
               New_assemblings = lists:delete( Worker, Assemblings ),
               New_completes = checkpoint_loop_release( New_assemblings, [Worker | Completes] ),
               checkpoint_loop( New_assemblings, New_completes )
        end.

checkpoint_loop_release( [], Completes ) ->
        [X ! all_complete || X <- Completes],
        [];
checkpoint_loop_release( _Assemblings, Completes ) -> Completes.

worker_loop( _Worker, 0, _Checkpoint ) -> ok;
worker_loop( Worker, N, Checkpoint ) ->
        Checkpoint ! {starting, erlang:self()},
        io:fwrite( "Worker ~p ~p~n", [Worker, N] ),
        timer:sleep( random:uniform(100) ),
        Checkpoint ! {done, erlang:self()},
        receive
        all_complete -> ok
        end,
        worker_loop( Worker, N - 1, Checkpoint ).
