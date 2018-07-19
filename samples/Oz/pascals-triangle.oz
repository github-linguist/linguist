declare
  fun {NextLine Xs}
     {List.zip 0|Xs {Append Xs [0]}
      fun {$ Left Right}
         Left + Right
      end}
  end

  fun {Triangle N}
     {List.take {Iterate [1] NextLine} N}
  end

  fun lazy {Iterate I F}
     I|{Iterate {F I} F}
  end

  %% Only works nicely for N =< 5.
  proc {PrintTriangle T}
     N = {Length T}
  in
     for
        Line in T
        Indent in N-1..0;~1
     do
        for _ in 1..Indent do {System.printInfo " "} end
        for L in Line do {System.printInfo L#" "} end
        {System.printInfo "\n"}
     end
  end
in
  {PrintTriangle {Triangle 5}}
