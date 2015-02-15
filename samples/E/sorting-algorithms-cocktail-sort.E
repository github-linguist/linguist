/** Cocktail sort (in-place) */
def cocktailSort(array) {
    def swapIndexes := 0..(array.size() - 2)
    def directions := [swapIndexes, swapIndexes.descending()]
    while (true) {
        for direction in directions {
            var swapped := false
            for a ? (array[a] > array[def b := a + 1]) in direction {
                def t    := array[a]
                array[a] := array[b]
                array[b] := t
                swapped  := true
            }
            if (!swapped) { return }
        }
    }
}
