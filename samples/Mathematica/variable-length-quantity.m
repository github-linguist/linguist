toOctets[n_Integer] :=
 StringJoin @@@
  Partition[
   PadLeft[Characters@IntegerString[n, 16],
    2 Ceiling[Plus @@ DigitCount[n, 16]/2], {"0"}], 2]

fromOctets[octets_List] := FromDigits[StringJoin @@ octets, 16]

Grid[{#, toOctets@#, fromOctets[toOctets@#]} & /@ {16^^3ffffe,
   16^^1fffff, 16^^200000}]
