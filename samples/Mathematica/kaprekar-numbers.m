KaprekaQ[1] = True;
KaprekaQ[n_Integer] :=  Block[{data = IntegerDigits[n^2], last = False, i = 1},
  While[i < Length[data] && FromDigits[data[[i + 1 ;;]]] =!= 0 &&  Not[last],
   last = FromDigits[data[[;; i]]] + FromDigits[data[[i + 1 ;;]]] == n;
   i++]; last];

Select[Range[10000], KaprekaQ]

Length[Select[Range[1000000], KaprekaQ]]
