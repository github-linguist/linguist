import parseutils, re, strutils

proc expandRange(input: string): string =
  var output: seq[string] = @[]
  for range in input.split(','):
    var sep = range.find('-', 1)
    if sep > 0: # parse range
      var first = -1
      if range.substr(0, sep-1).parseInt(first) == 0:
        break
      var last = -1
      if range.substr(sep+1).parseInt(last) == 0:
        break
      for i in first..last:
        output.add($i)
    else: # parse single number
      var n = -1
      if range.parseInt(n) > 0:
        output.add($n)
      else:
        break
  return output.join(",")

echo("-6,-3--1,3-5,7-11,14,15,17-20".expandRange)
