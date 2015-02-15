declare
  proc {Main}
     proc {Loop N Xs}
        if {Not {IsSorted Xs}} then
           Num NewXs
        in
           {System.printInfo N#": "}
           {System.print Xs}
           {System.printInfo " -- Reverse how many? "}
           Num = {String.toInt {ReadLine}}
           NewXs = {Append
                    {Reverse {List.take Xs Num}}
                    {List.drop Xs Num}}
           {Loop N+1 NewXs}
        else
           {System.showInfo "You took "#N#" tries to put the digits in order."}
        end
     end
     fun {EnsureShuffled Xs}
        Ys = {Shuffle Xs}
     in
        if {Not {IsSorted Ys}} then Ys
        else {EnsureShuffled Xs}
        end
     end
  in
     {Loop 0 {EnsureShuffled {List.number 1 9 1}}}
  end

  fun {IsSorted Xs}
     {Sort Xs Value.'<'} == Xs
  end

  local
     class TextFile from Open.file Open.text end
     StdIn = {New TextFile init(name:stdin)}
  in
     fun {ReadLine}
        {StdIn getS($)}
     end
  end

  fun {Shuffle Xs}
     {FoldL Xs
      fun {$ Z _}
         {Pick {Diff Xs Z}}|Z
      end
      nil}
  end

  fun {Pick Xs}
     {Nth Xs {OS.rand} mod {Length Xs} + 1}
  end

  fun {Diff Xs Ys}
     {FoldL Ys List.subtract Xs}
  end
in
  {Main}
