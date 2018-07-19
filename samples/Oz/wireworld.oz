declare
  Rules =
  [rule(&  & )
   rule(&H &t)
   rule(&t &.)
   rule(&. &H when:fun {$ Neighbours}
                      fun {IsHead X} X == &H end
                      Hs = {Filter Neighbours IsHead}
                      Len = {Length Hs}
                   in
                      Len == 1 orelse Len == 2
                   end)
   rule(&. &.)]

  Init = ["tH........."
          ".   .      "
          "   ...     "
          ".   .      "
          "Ht.. ......"]

  MaxGen = 100

  %% G(i) -> G(i+1)
  fun {Evolve Gi}
     fun {Get X#Y}
        Row = {CondSelect Gi Y unit}
     in
        {CondSelect Row X & } %% cells beyond boundaries are empty
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
             for Rule in Rules return:Return do
                if C == Rule.1 then
		   When = {CondSelect Rule when {Const true}}
		in
		   if {When {GetNeighbors X Y}} then
		      {Return Rule.2}
		   end
		end
	     end
          end}
      end}
  end

  %% Create an arena from a list of strings.
  fun {ReadArena LinesList}
     {List.toTuple '#'
      {Map LinesList
       fun {$ Line}
          {List.toTuple row Line}
       end}}
  end

  %% Converts an arena to a virtual string
  fun {ShowArena G}
     {Record.map G
      fun {$ L} {Record.toList L}#"\n" end}
  end

  %% helpers
  fun lazy {Iterate F V} V|{Iterate F {F V}} end
  fun {Const X} fun {$ _} X end end

  %% prepare GUI
  [QTk]={Module.link ["x-oz://system/wp/QTk.ozf"]}
  GenDisplay
  Field
  GUI = td(label(handle:GenDisplay)
           label(handle:Field font:{QTk.newFont font(family:'Courier')})
          )
  {{QTk.build GUI} show}

  G0 = {ReadArena Init}
  Gn = {Iterate Evolve G0}
in
  for
     Gi in Gn
     I in 0..MaxGen
  do
     {GenDisplay set(text:"Gen. "#I)}
     {Field set(text:{ShowArena Gi})}
     {Delay 500}
  end
