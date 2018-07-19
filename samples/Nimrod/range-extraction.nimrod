import parseutils, re, strutils

proc extractRange(input: string): string =
  var list = input.replace(re"\s+").split(',').map(parseInt)
  var ranges: seq[string] = @[]
  var i = 0
  while i < list.len:
    var first = list[i] # first element in the current range
    var offset = i
    while True: # skip ahead to the end of the current range
      if i + 1 >= list.len:
        # reached end of the list
        break
      if list[i + 1] - (i + 1) != first - offset:
        # next element isn't in the current range
        break
      i.inc
    var last = list[i] # last element in the current range
    case last - first
      of 0: ranges.add($first)
      of 1: ranges.add("$1,$2".format([$first, $last]))
      else: ranges.add("$1-$2".format([$first, $last]))
    i.inc
  return ranges.join(",")

echo("""
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39""".extractRange)
