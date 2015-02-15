-module( forest_fire ).

-export( [task/0] ).

-record( state, {neighbours=[], position, probability_burn, probability_grow, tree} ).

task() ->
       erlang:spawn( fun() ->
             Pid_positions = forest_create( 5, 5, 0.5, 0.3, 0.2 ),
             Pids = [X || {X, _} <- Pid_positions],
             [X ! {tree_pid_positions, Pid_positions} || X <- Pids],
             Start = forest_status( Pids ),
             Histories = [Start | [forest_step( Pids ) || _X <- lists:seq(1, 2)]],
             [io:fwrite("~p~n~n", [X]) || X <- Histories]
         end ).



forest_create( X_max, Y_max, Init, Grow, Burn ) ->
       [{tree_create(tree_init(Init, random:uniform()), X, Y, Grow, Burn), {X,Y}} || X <- lists:seq(1, X_max), Y<- lists:seq(1, Y_ma\
x)].

forest_status( Pids ) ->
       [X ! {status_request, erlang:self()} || X <- Pids],
       [receive {status, Tree, Position, X} -> {Tree, Position} end || X <- Pids].

forest_step( Pids ) ->
       [X ! {step} || X <- Pids],
       forest_status( Pids ).

is_neighbour({X, Y}, {X, Y} ) -> false; % Myself
is_neighbour({Xn, Yn}, {X, Y} ) when abs(Xn - X) =< 1, abs(Yn - Y) =< 1 -> true;
is_neighbour( _Position_neighbour, _Position ) -> false.

loop( State ) ->
        receive
        {tree_pid_positions, Pid_positions} ->
                loop( loop_neighbour(Pid_positions, State) );
        {step} ->
               [X ! {tree, State#state.tree, erlang:self()} || X <- State#state.neighbours],
               loop( loop_step(State) );
        {status_request, Pid} ->
                Pid ! {status, State#state.tree, State#state.position, erlang:self()},
                loop( State )
        end.

loop_neighbour(	Pid_positions, State ) ->
	My_position = State#state.position,
        State#state{neighbours=[Pid || {Pid, Position} <- Pid_positions, is_neighbour( Position, My_position)]}.

loop_step( State ) ->
        Is_burning = lists:any( fun loop_step_burning/1, [loop_step_receive(X) || X <- State#state.neighbours] ),
        Tree = loop_step_next( Is_burning, random:uniform(), State ),
        State#state{tree=Tree}.

loop_step_burning( Tree ) -> Tree =:= burning.

loop_step_next( _Is_burning, Probablility, #state{tree=empty, probability_grow=Grow} ) when Grow > Probablility -> tree;
loop_step_next( _Is_burning, _Probablility, #state{tree=empty} ) -> empty;
loop_step_next( _Is_burning, _Probablility, #state{tree=burning} ) -> empty;
loop_step_next( true, _Probablility, #state{tree=tree} ) -> burning;
loop_step_next( false, Probablility, #state{tree=tree, probability_burn=Burn} ) when Burn > Probablility  -> burning;
loop_step_next( false, _Probablility, #state{tree=tree} ) -> tree.

loop_step_receive( Pid ) -> receive {tree, Tree, Pid} -> Tree end.

tree_create( Tree, X, Y, Grow, Burn ) ->
        State = #state{position={X, Y}, probability_burn=Burn, probability_grow=Grow, tree=Tree},
        erlang:spawn_link( fun() -> random:seed( X, Y, 0 ), loop( State ) end ).

tree_init( Tree_probalility, Random ) when Tree_probalility > Random -> tree;
tree_init( _Tree_probalility, _Random ) -> empty.
