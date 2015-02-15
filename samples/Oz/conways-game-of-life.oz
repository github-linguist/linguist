declare
  Rules = [rule(c:1 n:[0 1]             new:0)  %% Lonely
           rule(c:1 n:[4 5 6 7 8]       new:0)  %% Overcrowded
           rule(c:1 n:[2 3]             new:1)  %% Lives
           rule(c:0 n:[3]               new:1)  %% It takes three to give birth!
           rule(c:0 n:[0 1 2 4 5 6 7 8] new:0)  %% Barren
          ]

  Blinker = ["..."
             "###"
             "..."]

  Toad = ["...."
          ".###"
          "###."
          "...."]

  Glider = [".#.........."
            "..#........."
            "###........."
            "............"
            "............"
            "............"
            "............"
            "............"
            "............"
            "............"
            "............"]

  Init = Blinker
  MaxGen = 2

  %% G(i) -> G(i+1)
  fun {Evolve Gi}
     fun {Get X#Y}
        Row = {CondSelect Gi Y unit}
     in
        {CondSelect Row X 0} %% cells beyond boundaries are dead (0)
     end
     fun {GetNeighbors X Y}
        {Map [X-1#Y-1  X#Y-1  X+1#Y-1
              X-1#Y           X+1#Y
              X-1#Y+1  X#Y+1  X+1#Y+1]
         Get}
     end
  in
     {Record.mapInd Gi
      fun {$ Y Row}
         {Record.mapInd Row
          fun {$ X C}
             N = {Sum {GetNeighbors X Y}}
          in
             for Rule in Rules return:Return do
                if C == Rule.c andthen {Member N Rule.n} then
                   {Return Rule.new}
                end
             end
          end}
      end}
  end

  %% For example: [".#"
  %%                "#."] -> grid(1:row(1:0 2:1) 2:row(1:1 2:0))
  fun {ReadG LinesList}
     {List.toTuple grid
      {Map LinesList
       fun {$ Line}
          {List.toTuple row
           {Map Line
            fun {$ C}
               if C == &. then 0
               elseif C == &# then 1
               end
            end}}
       end}}
  end

  %% Inverse to ReadG
  fun {ShowG G}
     {Map {Record.toList G}
      fun {$ Row}
         {Map {Record.toList Row}
          fun {$ C}
             if C == 0 then &.
             elseif C == 1 then &#
             end
          end}
      end}
  end

  %% Helpers
  fun {Sum Xs} {FoldL Xs Number.'+' 0} end
  fun lazy {Iterate F V} V|{Iterate F {F V}} end

  G0 = {ReadG Init}
  Gn = {Iterate Evolve G0}
in
  for
     Gi in Gn
     I in 0..MaxGen
  do
     {System.showInfo "\nGen. "#I}
     {ForAll {ShowG Gi} System.showInfo}
  end
