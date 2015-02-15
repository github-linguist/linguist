-module( maze_solving ).

-export( [task/0] ).

cells( {Start_x, Start_y}, {Stop_x, Stop_y}, Maze ) ->
    Start_pid = maze:cell_pid( Start_x, Start_y, Maze ),
    Stop_pid =  maze:cell_pid( Stop_x,  Stop_y, Maze ),
    {ok, Cells} = loop( Start_pid, Stop_pid, maze:cell_accessible_neighbours(Start_pid), [Start_pid] ),
    Cells.

task() ->
    Max_x = 16,
    Max_y = 8,
    Maze = maze:generation( Max_x, Max_y ),
    Start_x = random:uniform( Max_x ),
    Start_y = random:uniform( Max_y ),
    Stop_x = random:uniform( Max_x ),
    Stop_y = random:uniform( Max_y ),
    Cells = cells( {Start_x, Start_y}, {Stop_x, Stop_y}, Maze ),
    [maze:cell_content_set(X, ".") || X <- Cells],
    maze:cell_content_set( maze:cell_pid(Start_x, Start_y, Maze), "S" ),
    maze:cell_content_set( maze:cell_pid(Stop_x, Stop_y, Maze), "G" ),
    maze:display( Maze ),
    maze:stop( Maze ).



loop( _Start, _Stop, [], _Acc) -> {error, dead_end};
loop( _Start, Stop, [Stop], Acc ) -> {ok, lists:reverse( [Stop | Acc] )};
loop( Start, Stop, [Next], [Previous | _T]=Acc ) -> loop( Start, Stop, lists:delete(Previous, maze:cell_accessible_neighbours(Next)), [Next | Acc] );
loop( Start, Stop, Nexts, Acc ) -> loop_stop( lists:member(Stop, Nexts), Start, Stop, Nexts, Acc ).

loop_stop( true, _Start, Stop, _Nexts, Acc ) -> {ok, lists:reverse( [Stop | Acc] )};
loop_stop( false, Start, Stop, Nexts, Acc ) ->
        My_pid = erlang:self(),
        [erlang:spawn( fun() -> My_pid ! loop( Start, Stop, [X], Acc ) end ) || X <- Nexts],
        receive
        {ok, Cells} -> {ok, Cells}
        end.
