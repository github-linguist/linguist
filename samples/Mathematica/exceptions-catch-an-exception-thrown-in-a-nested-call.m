foo[] := Catch[ bar[1]; bar[2]; ]

bar[i_] := baz[i];

baz[i_] := Switch[i,
  1, Throw["Exception U0 in baz"];,
  2, Throw["Exception U1 in baz"];]
