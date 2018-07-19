def bubbleSort(target) {
  __loop(fn {
    var changed := false
    for i in 0..(target.size() - 2) {
      def [a, b] := target(i, i + 2)
      if (a > b) {
        target(i, i + 2) := [b, a]
        changed := true
      }
    }
    changed
  })
}
