declare
  fun {ZipRecord Keys Values}
     {List.toRecord unit {List.zip Keys Values MakePair}}
  end

  fun {MakePair A B}
     A#B
  end
in
  {Show {ZipRecord [a b c] [1 2 3]}}
