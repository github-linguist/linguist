declare
  fun {Square X} X*X end

  fun {RMS Xs}
     {Sqrt
      {Int.toFloat {FoldL {Map Xs Square} Number.'+' 0}}
      /
      {Int.toFloat {Length Xs}}}
  end
in
  {Show {RMS {List.number 1 10 1}}}
