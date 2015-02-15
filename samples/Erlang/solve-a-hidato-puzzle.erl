-module( solve_hidato_puzzle ).

-export( [create/2, solve/1, task/0] ).

-compile({no_auto_import,[max/2]}).

create( Grid_list, Number_list ) ->
        Squares = lists:flatten( [create_column(X, Y) || {X, Y} <- Grid_list] ),
	lists:foldl( fun store/2, dict:from_list(Squares), Number_list ).

print( Grid_list ) when is_list(Grid_list) -> print( create(Grid_list, []) );
print( Grid_dict ) ->
    Max_x = max_x( Grid_dict ),
    Max_y = max_y( Grid_dict ),
    Print_row = fun (Y) -> [print(X, Y, Grid_dict) || X <- lists:seq(1, Max_x)], io:nl() end,
    [Print_row(Y) || Y <- lists:seq(1, Max_y)].

solve( Dict ) ->
    {find_start, [Start]} = {find_start, dict:fold( fun start/3, [], Dict )},
    Max = dict:size( Dict ),
    {stop_ok, {Max, Max, [Stop]}} = {stop_ok, dict:fold( fun stop/3, {Max, 0, []}, Dict )},
    My_pid = erlang:self(),
    erlang:spawn( fun() -> path(Start, Stop, Dict, My_pid, []) end ),
    receive
    {grid, Grid, path, Path} -> {Grid, Path}
    end.

task() ->
    %% Square is {X, Y}, N}. N = 0 for empty square. These are created if not present.
    %% Leftmost column is X=1. Top row is Y=1.
    %% Optimised for the example, grid is a list of {X, {Y_min, Y_max}}.
    %% When there are holes, X is repeated as many times as needed with two new Y values each time.
    Start = {{7,5}, 1},
    Stop = {{5,4}, 40},
    Grid_list = [{1, {1,5}}, {2, {1,5}}, {3, {1,6}}, {4, {1,6}}, {5, {1,7}}, {6, {3,7}}, {7, {5,8}}, {8, {7,8}}],
    Number_list = [Start, Stop, {{1,5}, 27}, {{2,1}, 33}, {{2,4}, 26}, {{3,1}, 35}, {{3,2}, 24},
                {{4,2}, 22}, {{4,3}, 21}, {{4,4}, 13}, {{5,5}, 9}, {{5,6}, 18}, {{6,4}, 11}, {{6,7}, 7}, {{7,8}, 5}],
    Grid = create( Grid_list, Number_list ),
    io:fwrite( "Start grid~n" ),
    print( Grid ),
    {New_grid, Path} = solve( create(Grid_list, Number_list) ),
    io:fwrite( "Start square ~p, Stop square ~p.~nPath ~p~n", [Start, Stop, Path] ),
    print( New_grid ).


create_column( X, {Y_min, Y_max} ) -> [{{X, Y}, 0} || Y <- lists:seq(Y_min, Y_max)].

is_filled( Dict ) -> [] =:= dict:fold( fun keep_0_square/3, [], Dict ).

keep_0_square( Key, 0, Acc ) -> [Key | Acc];
keep_0_square(  _Key, _Value, Acc ) -> Acc.

max( Position, Keys ) ->
    [Square | _T] = lists:reverse( lists:keysort(Position, Keys) ),
    Square.

max_x( Dict ) ->
    {X, _Y} = max( 1, dict:fetch_keys(Dict) ),
    X.

max_y( Dict ) ->
    {_X, Y} = max( 2, dict:fetch_keys(Dict) ),
    Y.


neighbourhood( Square, Dict ) ->
        Potentials = neighbourhood_potential_squares( Square ),
	neighbourhood_squares( dict:find(Square, Dict), Potentials, Dict ).

neighbourhood_potential_squares( {X, Y} ) -> [{Sx, Sy} || Sx <- [X-1, X, X+1], Sy <- [Y-1, Y, Y+1], {X, Y} =/= {Sx, Sy}].

neighbourhood_squares( {ok, Value}, Potentials, Dict ) ->
        Square_values = lists:flatten( [neighbourhood_square_value(X, dict:find(X, Dict)) || X <- Potentials] ),
        Next_value = Value + 1,
        neighbourhood_squares_next_value( lists:keyfind(Next_value, 2, Square_values), Square_values, Next_value ).

neighbourhood_squares_next_value( {Square, Value}, _Square_values, Value ) -> [{Square, Value}];
neighbourhood_squares_next_value( false, Square_values, Value ) -> [{Square, Value} || {Square, Y} <- Square_values, Y =:= 0].

neighbourhood_square_value( Square, {ok, Value} ) -> [{Square, Value}];
neighbourhood_square_value( _Square, error ) -> [].

path( Square, Square, Dict, Pid, Path ) -> path_correct( is_filled(Dict), Pid, [Square | Path], Dict );
path( Square, Stop, Dict, Pid, Path ) ->
    Reversed_path = [Square | Path],
    Neighbours = neighbourhood( Square, Dict ),
    [erlang:spawn( fun() -> path(Next_square, Stop, dict:store(Next_square, Value, Dict), Pid, Reversed_path) end ) || {Next_square, Value} <- Neighbours].

path_correct( true, Pid, Path, Dict ) -> Pid ! {grid, Dict, path, lists:reverse( Path )};
path_correct( false, _Pid, _Path, _Dict ) -> dead_end.

print( X, Y, Dict ) -> print_number( dict:find({X, Y}, Dict) ).

print_number( {ok, 0} ) -> io:fwrite( "~3s", ["."] ); % . is less distracting than 0
print_number( {ok, Value} ) -> io:fwrite( "~3b", [Value] );
print_number( error ) -> io:fwrite( "~3s", [" "] ).

start( Key, 1, Acc ) -> [Key | Acc]; % Allow check that we only have one key with value 1.
start( _Key, _Value, Acc ) -> Acc.

stop( Key, Max, {Max, Max_found, Stops} ) -> {Max, erlang:max(Max, Max_found), [Key | Stops]}; % Allow check that we only have one key with value Max.
stop( _Key, Value, {Max, Max_found, Stops} ) -> {Max, erlang:max(Value, Max_found), Stops}. % Allow check that Max is Max.

store( {Key, Value}, Dict ) -> dict:store( Key, Value, Dict ).
