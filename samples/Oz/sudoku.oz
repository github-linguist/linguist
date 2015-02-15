declare
  %% a puzzle is a function that returns an initial board configuration
  fun {Puzzle1}
     %% a board is a list of 9 rows
     [[4 _ _  _ _ _  _ 6 _]
      [5 _ _  _ 8 _  9 _ _]
      [3 _ _  _ _ 1  _ _ _]

      [_ 2 _  7 _ _  _ _ 1]
      [_ 9 _  _ _ _  _ 4 _]
      [8 _ _  _ _ 3  _ 5 _]

      [_ _ _  2 _ _  _ _ 7]
      [_ _ 6  _ 5 _  _ _ 8]
      [_ 1 _  _ _ _  _ _ 6]]
  end

  %% Returns a list of solutions for the given puzzle.
  fun {Solve Puzzle}
     {SearchAll {GetScript Puzzle}}
  end

  %% Creates a solver script for a puzzle.
  fun {GetScript Puzzle}
     proc {$ Board}
        %% Every row is a list of nine finite domain vars
        %% with the domain 1..9.
        Board = {MapRange fun {$ _} {FD.list 9 1#9} end}
        %% Post initial configuration.
        Board = {Puzzle}

        %% The core constraints:
        {ForAll {Rows Board} FD.distinct}
        {ForAll {Columns Board} FD.distinct}
        {ForAll {Boxes Board} FD.distinct}

        %% Search if necessary.
        {FD.distribute ff {Flatten Board}}
     end
  end

  %% Returns the board as a list of rows.
  fun {Rows Board}
     Board %% This is already the representation we have chosen.
  end

  %% Returns the board as a list of columns.
  fun {Columns Board}
     {MapRange fun {$ I} {Column Board I} end}
  end

  %% Returns the board as a list of boxes (sub-grids).
  fun {Boxes Board}
     {MapRange fun {$ I} {Box Board I} end}
  end

  %% Helper function: map the range 1..9 to something.
  fun {MapRange F}
     {Map [1 2 3 4 5 6 7 8 9] F}
  end

  %% Returns a column of the board as a list of fields.
  fun {Column Board Index}
     {Map Board
      fun {$ Row}
         {Nth Row Index}
      end
     }
  end

  %% Returns a box of the board as a list of fields.
  fun {Box Board Index}
     Index0 = Index-1
     Fields = {Flatten Board}
     Start = (Index0 div 3) * 27 + (Index0 mod 3)*3
  in
     {Flatten
      for I in 0..2 collect:C do
         {C {List.take {List.drop Fields Start+I*9} 3}}
      end
     }
  end
in
  {Inspect {Solve Puzzle1}.1}
