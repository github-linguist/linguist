declare
  fun {Mean Xs}
     {FoldL Xs Number.'+' 0.0} / {Int.toFloat {Length Xs}}
  end
in
  {Show {Mean [3. 1. 4. 1. 5. 9.]}}
