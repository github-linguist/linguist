countingSort[list_] := Module[{minElem, maxElem, count, z, number},
  minElem = Min[list]; maxElem = Max[list];
  count = ConstantArray[0, (maxElem - minElem + 1)];
  For[number = 1, number < Length[list], number++,
   count[[number - minElem + 1]] = count[[number - minElem + 1]] + 1;] ;
  z = 1;
  For[i = minElem, i < maxElem, i++,
   While[count[[i - minElem + 1]] > 0,
    list[[z]] = i; z++;
    count[[i - minElem + 1]] = count[[i - minElem + 1]] - 1;]
   ];
  ]
