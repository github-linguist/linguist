def swap(container, ixA, ixB) {
    def temp := container[ixA]
    container[ixA] := container[ixB]
    container[ixB] := temp
}

/** Reverse order of elements of 'sequence' whose indexes are in the interval [ixLow, ixHigh] */
def reverseRange(sequence, var ixLow, var ixHigh) {
    while (ixLow < ixHigh) {
        swap(sequence, ixLow, ixHigh)
        ixLow += 1
        ixHigh -= 1
    }
}

/** Algorithm from <http://marknelson.us/2002/03/01/next-permutation>, allegedly from a version of the C++ STL */
def nextPermutation(sequence) {
    def last := sequence.size() - 1
    var i := last
    while (true) {
        var ii := i
        i -= 1
        if (sequence[i] < sequence[ii]) {
            var j := last + 1
            while (!(sequence[i] < sequence[j -= 1])) {} # buried side effect
            swap(sequence, i, j)
            reverseRange(sequence, ii, last)
            return true
        }
        if (i == 0) {
            reverseRange(sequence, 0, last)
            return false
        }
    }
}

/** Note: Worst case on sorted list */
def permutationSort(flexList) {
    while (nextPermutation(flexList)) {}
}
