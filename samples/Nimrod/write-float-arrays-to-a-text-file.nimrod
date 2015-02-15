import strutils, math, sequtils

const
   outFileName = "floatarr2file.txt"

proc sqrt*(x: int64): float {.importc: "sqrt", header: "<math.h>".}

const
   xprecision = 3
   yprecision = 5

var a: seq[int64] = @[int64(1), 2, 3, 100_000_000_000]
var b: seq[float] = @[sqrt(a[0]), sqrt(a[1]), sqrt(a[2]), sqrt(a[3])]
var c = zip(a,b)
var res: string = ""
for t in c:
    res.add($formatFloat(float(t.a),ffDefault,xprecision) & "\t" & $formatFloat(t.b,ffDefault,yprecision) & "\n")

writeFile(outFileName,res)
var res2 = readFile(outFileName)
echo(res2)
