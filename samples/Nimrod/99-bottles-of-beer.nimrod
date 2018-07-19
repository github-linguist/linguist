proc GetBottleNumber(n: int): string =
  var bs: string
  if n == 0:
    bs = "No more bottles"
  elif n == 1:
    bs = "1 bottle"
  else:
    bs = $n & " bottles"
  return bs & " of beer"

for bn in countdown(99, 1):
  var cur = GetBottleNumber(bn)
  echo(cur, " on the wall, ", cur, ".")
  echo("Take one down and pass it around, ", GetBottleNumber(bn-1), " on the wall.\n")

echo "No more bottles of beer on the wall, no more bottles of beer."
echo "Go to the store and buy some more, 99 bottles of beer on the wall."
