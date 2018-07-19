n = 2
m = 3
StringTake["Mathematica", {n+1, n+m-1}]

StringDrop["Mathematica", n]

(* StringPosition returns a list of starting and ending character positions for a substring *)
pos = StringPosition["Mathematica", "e"][[1]][[1]]
StringTake["Mathematica", {pos, pos+m-1}]

(* Similar to above *)
pos = StringPosition["Mathematica", "the"][[1]]
StringTake["Mathematica", {pos, pos+m-1}]
