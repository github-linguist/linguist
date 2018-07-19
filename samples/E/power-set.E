pragma.enable("accumulator")

def powerset(s) {
  return accum [].asSet() for k in 0..!2**s.size() {
    _.with(accum [].asSet() for i ? ((2**i & k) > 0) => elem in s {
      _.with(elem)
    })
  }
}
