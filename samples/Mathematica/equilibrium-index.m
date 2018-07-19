equilibriumIndex[data_]:=Reap[
    Do[If[Total[data[[;; n - 1]]] == Total[data[[n + 1 ;;]]],Sow[n]],
    {n, Length[data]}]][[2, 1]]
