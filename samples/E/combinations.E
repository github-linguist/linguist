def combinations(m, range) {
  return if (m <=> 0) { [[]] } else {
    def combGenerator {
      to iterate(f) {
        for i in range {
          for suffix in combinations(m.previous(), range & (int > i)) {
            f(null, [i] + suffix)
          }
        }
      }
    }
  }
}
