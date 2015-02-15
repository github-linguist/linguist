declare
  fun {Queens N}
     proc {$ Board}
        %% a board is a N-tuple of rows
        Board = {MakeTuple queens N}
        for Y in 1..N  do
           %% a row is a N-tuple of values in [0,1]
           %% (0: no queen, 1: queen)
           Board.Y = {FD.tuple row N 0#1}
        end

        {ForAll {Rows Board} SumIs1}
        {ForAll {Columns Board} SumIs1}

        %% for every two points on a diagonal
        for [X1#Y1 X2#Y2] in {DiagonalPairs N} do
           %$ at most one of them has a queen
           Board.Y1.X1 + Board.Y2.X2 =<: 1
        end

        %% enumerate all such boards
        {FD.distribute naive {FlatBoard Board}}
     end
  end

  fun {Rows Board}
     {Record.toList Board}
  end

  fun {Columns Board}
     for X in {Arity Board.1} collect:C1 do
        {C1
         for Y in {Arity Board} collect:C2 do
            {C2 Board.Y.X}
         end}
     end
  end

  proc {SumIs1 Xs}
     {FD.sum Xs '=:' 1}
  end

  fun {DiagonalPairs N}
     proc {Coords Root}
        [X1#Y1 X2#Y2] = Root
        Diff
     in
        X1::1#N Y1::1#N
        X2::1#N Y2::1#N
        %% (X1,Y1) and (X2,Y2) are on a diagonal if {Abs X2-X1} = {Abs Y2-Y1}
        Diff::1#N-1
        {FD.distance X2 X1 '=:' Diff}
        {FD.distance Y2 Y1 '=:' Diff}
        %% enumerate all such coordinates
        {FD.distribute naive [X1 Y1 X2 Y2]}
     end
  in
     {SearchAll Coords}
  end

  fun {FlatBoard Board}
     {Flatten {Record.toList {Record.map Board Record.toList}}}
  end

  Solutions = {SearchAll {Queens 8}}
in
  {Length Solutions} = 92 %% assert
  {Inspect {List.take Solutions 3}}
