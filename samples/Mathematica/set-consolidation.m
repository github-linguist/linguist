reduce[x_] :=
 Block[{pairs, unique},
  pairs =
   DeleteCases[
    Subsets[Range@
      Length@x, {2}], _?(Intersection @@ x[[#]] == {} &)];
  unique = Complement[Range@Length@x, Flatten@pairs];
  Join[Union[Flatten[x[[#]]]] & /@ pairs, x[[unique]]]]

consolidate[x__] := FixedPoint[reduce, {x}]
