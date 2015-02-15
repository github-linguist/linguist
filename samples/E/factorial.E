pragma.enable("accumulator")
def factorial(n) {
  return accum 1 for i in 2..n { _ * i }
}
