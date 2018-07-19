/** returns a if it is nonzero, otherwise b() */
def nonzeroOr(a, b) { return if (a.isZero()) { b() } else { a } }

["Here", "are", "some", "sample", "strings", "to", "be", "sorted"] \
    .sort(fn a, b {
              nonzeroOr(b.size().op__cmp(a.size()),
                        fn { a.compareToIgnoreCase(b) })
          })
