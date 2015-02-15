pragma.enable("accumulator")
def isPerfectNumber(x :int) {
  var sum := 0
  for d ? (x % d <=> 0) in 1..!x {
    sum += d
    if (sum > x) { return false }
  }
  return sum <=> x
}
