pragma.enable("accumulator")
def reverse(string) {
  return accum "" for i in (0..!(string.size())).descending() { _ + string[i] }
}
