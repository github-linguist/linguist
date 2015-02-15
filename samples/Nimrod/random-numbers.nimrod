import math, strutils

const  precisn = 5
var rs: TRunningStat

proc normGauss: float {.inline.} = 1 + 0.76 * cos(2*PI*random(1.0)) * sqrt(-2*log10(random(1.0)))

randomize()

for j in 0..5:
   for i in 0..1000:
      rs.push(normGauss())
   echo("mean: ", $formatFloat(rs.mean,ffDecimal,precisn),
        " stdDev: ", $formatFloat(rs.standardDeviation(),ffDecimal,precisn))
