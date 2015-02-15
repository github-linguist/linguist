cocktailSort[A_List] := Module[ { swapped = True },
While[ swapped == True,
 swapped=False;
 For[ i = 1, i< Length[A]-1,i++,
   If[ A[[i]] > A[[i+1]], A[[i;;i+1]] = A[[i+1;;i;;-1]]; swapped=True;]
 ];
If[swapped == False, Break[]];
swapped=False;
For [ i= Length[A]-1, i > 0, i--,
  If[ A[[i]] > A[[i+1]], A[[i;;i+1]] = A[[i+1;;i;;-1]]; swapped = True;]
 ]]]
