declare
  proc {Main}
     Solution = {PickNUnique 4 {List.number 1 9 1}}

     proc {Loop}
        Guess = {EnterGuess}
     in
        {System.showInfo
         {Bulls Guess Solution}#" bulls and "#
         {Cows Guess Solution}#" cows"}
        if Guess \= Solution then {Loop} end
     end
  in
     {Loop}
     {System.showInfo "You have won!"}
  end

  fun {Bulls Xs Sol}
     {Length {Filter {List.zip Xs Sol Value.'=='} Id}}
  end

  fun {Cows Xs Sol}
     {Length {Intersection Xs Sol}}
  end

  local
     class TextFile from Open.file Open.text end
     StdIn = {New TextFile init(name:stdin)}
  in
     fun {EnterGuess}
        try
           {System.printInfo "Enter your guess (e.g. \"1234\"): "}
           S = {StdIn getS($)}
        in
           %% verify
           {Length S} = 4
           {All S Char.isDigit} = true
           {FD.distinct S} %% assert there is no duplicate digit
           %% convert from digits to numbers
           {Map S fun {$ D} D-&0 end}
        catch _ then
           {EnterGuess}
        end
     end
  end

  fun {PickNUnique N Xs}
     {FoldL {MakeList N}
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

  fun {Intersection Xs Ys}
     {Filter Xs fun {$ X} {Member X Ys} end}
  end

  fun {Id X} X end
in
  {Main}
