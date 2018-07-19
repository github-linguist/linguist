def isHappyNumber(var x :int) {
  var seen := [].asSet()
  while (!seen.contains(x)) {
    seen with= x
    var sum := 0
    while (x > 0) {
      sum += (x % 10) ** 2
      x //= 10
    }
    x := sum
    if (x == 1) { return true }
  }
  return false
}

var count := 0
for x ? (isHappyNumber(x)) in (int >= 1) {
  println(x)
  if ((count += 1) >= 8) { break }
}
