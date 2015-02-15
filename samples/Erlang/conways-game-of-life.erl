-module(life).

-export([bang/1]).


-define(CHAR_DEAD,   32).  % " "
-define(CHAR_ALIVE, 111).  % "o"
-define(CHAR_BAR,    45).  % "-"

-define(GEN_INTERVAL, 100).


-record(state, {x            :: non_neg_integer()
               ,y            :: non_neg_integer()
               ,n            :: pos_integer()
               ,bar          :: nonempty_string()
               ,board        :: array()
               ,gen_count    :: pos_integer()
               ,gen_duration :: non_neg_integer()
               ,print_time   :: non_neg_integer()
               }).


%% ============================================================================
%% API
%% ============================================================================

bang(Args) ->
    [X, Y] = [atom_to_integer(A) || A <- Args],
    {Time, Board} = timer:tc(fun() -> init_board(X, Y) end),
    State = #state{x            = X
                  ,y            = Y
                  ,n            = X * Y
                  ,bar          = [?CHAR_BAR || _ <- lists:seq(1, X)]
                  ,board        = Board
                  ,gen_count    = 1  % Consider inital state to be generation 1
                  ,gen_duration = Time
                  ,print_time   = 0  % There was no print time yet
    },
    life_loop(State).


%% ============================================================================
%% Internal
%% ============================================================================

life_loop(
    #state{x            = X
          ,y            = Y
          ,n            = N
          ,bar          = Bar
          ,board        = Board
          ,gen_count    = GenCount
          ,gen_duration = Time
          ,print_time   = LastPrintTime
    }=State) ->

    {PrintTime, ok} = timer:tc(
        fun() ->
            do_print_screen(Board, Bar, X, Y, N, GenCount, Time, LastPrintTime)
        end
    ),

    {NewTime, NewBoard} = timer:tc(
        fun() ->
            next_generation(X, Y, Board)
        end
    ),

    NewState = State#state{board        = NewBoard
                          ,gen_count    = GenCount + 1
                          ,gen_duration = NewTime
                          ,print_time   = PrintTime
    },

    NewTimeMil = NewTime / 1000,
    NextGenDelay = at_least_zero(round(?GEN_INTERVAL - NewTimeMil)),
    timer:sleep(NextGenDelay),

    life_loop(NewState).


at_least_zero(Integer) when Integer >= 0 -> Integer;
at_least_zero(_) -> 0.


do_print_screen(Board, Bar, X, Y, N, GenCount, Time, PrintTime) ->
    ok = do_print_status(Bar, X, Y, N, GenCount, Time, PrintTime),
    ok = do_print_board(Board).


do_print_status(Bar, X, Y, N, GenCount, TimeMic, PrintTimeMic) ->
    TimeSec = TimeMic / 1000000,
    PrintTimeSec = PrintTimeMic / 1000000,
    ok = io:format("~s~n", [Bar]),
    ok = io:format(
        "X: ~b Y: ~b CELLS: ~b GENERATION: ~b DURATION: ~f PRINT TIME: ~f~n",
        [X, Y, N, GenCount, TimeSec, PrintTimeSec]
    ),
    ok = io:format("~s~n", [Bar]).


do_print_board(Board) ->
    % It seems that just doing a fold should be faster than map + to_list
    % combo, but, after measuring several times, map + to_list has been
    % consistently (nearly twice) faster than either foldl or foldr.
    RowStrings = array:to_list(
        array:map(
            fun(_, Row) ->
                array:to_list(
                    array:map(
                        fun(_, State) ->
                            state_to_char(State)
                        end,
                        Row
                    )
                )
            end,
            Board
        )
    ),

    ok = lists:foreach(
        fun(RowString) ->
            ok = io:format("~s~n", [RowString])
        end,
        RowStrings
    ).


state_to_char(0) -> ?CHAR_DEAD;
state_to_char(1) -> ?CHAR_ALIVE.


next_generation(W, H, Board) ->
    array:map(
        fun(Y, Row) ->
            array:map(
                fun(X, State) ->
                    Neighbors = filter_offsides(H, W, neighbors(X, Y)),
                    States = neighbor_states(Board, Neighbors),
                    LiveNeighbors = lists:sum(States),
                    new_state(State, LiveNeighbors)
                end,
                Row
            )
        end,
        Board
    ).


new_state(1, LiveNeighbors) when LiveNeighbors  <  2 -> 0;
new_state(1, LiveNeighbors) when LiveNeighbors  <  4 -> 1;
new_state(1, LiveNeighbors) when LiveNeighbors  >  3 -> 0;
new_state(0, LiveNeighbors) when LiveNeighbors =:= 3 -> 1;
new_state(State, _LiveNeighbors) -> State.


neighbor_states(Board, Neighbors) ->
    [array:get(X, array:get(Y, Board)) || {X, Y} <- Neighbors].


filter_offsides(H, W, Coordinates) ->
    [{X, Y} || {X, Y} <- Coordinates, is_onside(X, Y, H, W)].


is_onside(X, Y, H, W) when (X >= 0) and (Y >= 0) and (X < W) and (Y < H) -> true;
is_onside(_, _, _, _) -> false.


neighbors(X, Y) ->
    [{X + OffX, Y + OffY} || {OffX, OffY} <- offsets()].


offsets() ->
    [offset(D) || D <- directions()].


offset('N')  -> { 0, -1};
offset('NE') -> { 1, -1};
offset('E')  -> { 1,  0};
offset('SE') -> { 1,  1};
offset('S')  -> { 0,  1};
offset('SW') -> {-1,  1};
offset('W')  -> {-1,  0};
offset('NW') -> {-1, -1}.


directions() ->
    ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'].


init_board(X, Y) ->
    array:map(fun(_, _) -> init_row(X) end, array:new(Y)).


init_row(X) ->
    array:map(fun(_, _) -> init_cell_state() end, array:new(X)).


init_cell_state() ->
    crypto:rand_uniform(0, 2).


atom_to_integer(Atom) ->
    list_to_integer(atom_to_list(Atom)).
