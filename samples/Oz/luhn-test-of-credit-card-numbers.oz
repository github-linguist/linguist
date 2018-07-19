declare
  fun {Luhn N}
     {Sum {List.mapInd {Reverse {Digits N}}
           fun {$ Idx Dig}
              if {IsEven Idx} then {Sum {Digits 2*Dig}}
              else Dig
              end
           end}}
     mod 10 == 0
  end

  fun {Digits N}
     {Map {Int.toString N} fun {$ D} D - &0 end}
  end

  fun {Sum Xs}
     {FoldL Xs Number.'+' 0}
  end
in
  {Show
   {Map
    [49927398716 49927398717 1234567812345678 1234567812345670]
    Luhn}}
