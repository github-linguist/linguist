-module(metered).
-compile(export_all).

create_semaphore(N) ->
    spawn(?MODULE, sem_loop, [N,N]).

sem_loop(0,Max) ->
    io:format("Resources exhausted~n"),
    receive
        {release, PID} ->
            PID ! released,
            sem_loop(1,Max);
        {stop, _PID} ->
            ok
    end;
sem_loop(N,N) ->
    receive
        {acquire, PID} ->
            PID ! acquired,
            sem_loop(N-1,N);
        {stop, _PID} ->
            ok
    end;
sem_loop(N,Max) ->
    receive
        {release, PID} ->
            PID ! released,
            sem_loop(N+1,Max);
        {acquire, PID} ->
            PID ! acquired,
            sem_loop(N-1,Max);
        {stop, _PID} ->
            ok
    end.

release(Sem) ->
    Sem ! {release, self()},
    receive
        released ->
            ok
    end.
acquire(Sem) ->
    Sem ! {acquire, self()},
    receive
        acquired ->
            ok
    end.

start() -> create_semaphore(10).

stop(Sem) -> Sem ! {stop, self()}.

worker(P,N,Sem) ->
    acquire(Sem),
    io:format("Worker ~b has the acquired semaphore~n",[N]),
    timer:sleep(500 * random:uniform(4)),
    release(Sem),
    io:format("Worker ~b has released the semaphore~n",[N]),
    P ! {done, self()}.

test() ->
    Sem = start(),
    Pids = lists:map(fun (N) ->
                             spawn(?MODULE, worker, [self(),N,Sem])
                     end, lists:seq(1,20)),
    lists:foreach(fun (P) -> receive {done, P} -> ok end end, Pids),
    stop(Sem).
