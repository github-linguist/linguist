/** Shell sort (in-place) */
def shellSort(array) {
    var inc := array.size() // 2
    while (inc.aboveZero()) {
        for var i => a in array {
            while (i >= inc && (def b := array[i - inc]) > a) {
                array[i] := b
                i -= inc
            }
            array[i] := a
        }
        inc := if (inc <=> 2) { 1 } else { (inc * 5.0 / 11).floor() }
    }
}
