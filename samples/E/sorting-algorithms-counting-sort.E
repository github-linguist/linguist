def countingSort(array, min, max) {
    def counts := ([0] * (max - min + 1)).diverge()
    for elem in array {
        counts[elem - min] += 1
    }
    var i := -1
    for offset => count in counts {
        def elem := min + offset
        for _ in 1..count {
            array[i += 1] := elem
        }
    }
}
