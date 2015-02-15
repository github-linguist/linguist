MergeSort[m_List] := Module[{middle},
  If[Length[m] >= 2,
   middle = Ceiling[Length[m]/2];
   Apply[Merge,
    Map[MergeSort, Partition[m, middle, middle, {1, 1}, {}]]],
   m
   ]
  ]

Merge[left_List, right_List] := Module[
  {leftIndex = 1, rightIndex = 1},
  Table[
   Which[
    leftIndex > Length[left], right[[rightIndex++]],
    rightIndex > Length[right], left[[leftIndex++]],
    left[[leftIndex]] <= right[[rightIndex]], left[[leftIndex++]],
    True, right[[rightIndex++]]],
   {Length[left] + Length[right]}]
  ]
