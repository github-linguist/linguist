declare
  fun {DotProduct Xs Ys}
     {Length Xs} = {Length Ys} %% assert
     {List.foldL {List.zip Xs Ys Number.'*'} Number.'+' 0}
  end
in
  {Show {DotProduct [1 3 ~5] [4 ~2 ~1]}}
