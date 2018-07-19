declare
  %% A carpet is a list of lines.
  fun {NextCarpet Carpet}
     {Flatten
      [{Map Carpet XXX}
       {Map Carpet X_X}
       {Map Carpet XXX}
      ]}
  end

  fun {XXX X} X#X#X end
  fun {X_X X} X#{Spaces {VirtualString.length X}}#X end
  fun {Spaces N} if N == 0 then nil else & |{Spaces N-1} end end

  fun lazy {Iterate F X}
     X|{Iterate F {F X}}
  end

  SierpinskiCarpets = {Iterate NextCarpet ["#"]}
in
  %% print all lines of the Sierpinski carpet of order 3
  {ForAll {Nth SierpinskiCarpets 4} System.showInfo}
