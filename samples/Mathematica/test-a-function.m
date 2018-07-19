myFun[x_] := Block[{y},y = x^2; Assert[y > 5]; Sin[y]]
On[Assert];myFun[1.0]
