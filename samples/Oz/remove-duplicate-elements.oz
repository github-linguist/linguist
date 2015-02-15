declare

  fun {Nub Xs}
     D = {Dictionary.new}
  in
     for X in Xs do D.X := unit end
     {Dictionary.keys D}
  end

in

  {Show {Nub [1 2 1 3 5 4 3 4 4]}}
