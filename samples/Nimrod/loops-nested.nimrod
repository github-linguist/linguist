import math, strutils

const arrSize = 10

var a: array[0..arrSize-1, array[0..arrSize-1, int]]
var s: string = ""

randomize()   # different results each time this runs

for i in 0 .. arrSize-1:
   for j in countup(0,arrSize-1):
      a[i][j] = random(20)+1

block outer:
   for i in countup(0,arrSize-1):
      for j in 0 .. arrSize-1:
         if a[i][j] < 10:
            s.add(" ")
         addf(s,"$#",$a[i][j])
         if a[i][j] == 20:
            break outer
         s.add(", ")
      s.add("\n")
echo(s)
