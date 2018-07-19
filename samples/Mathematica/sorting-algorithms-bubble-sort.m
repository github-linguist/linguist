bubbleSort[{w___, x_, y_, z___}] /; x > y := bubbleSort[{w, y, x, z}]
bubbleSort[sortedList_] := sortedList
