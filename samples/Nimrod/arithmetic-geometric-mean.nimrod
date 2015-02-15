import math

proc agm(a, g: float,delta: float = 1.0e-15): float =
  var
    aNew: float = 0
    aOld: float = a
    gOld: float = g
  while (abs(aOld - gOld) > delta):
    aNew = 0.5 * (aOld + gOld)
    gOld = sqrt(aOld * gOld)
    aOld = aNew
  result = aOld

echo ($agm(1.0,1.0/sqrt(2)))
