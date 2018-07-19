/** Returns null if the value is not found. */
def binarySearch(collection, value) {
    var low := 0
    var high := collection.size() - 1
    while (low <= high) {
        def mid := (low + high) // 2
        def comparison := value.op__cmp(collection[mid])
        if      (comparison.belowZero()) { high := mid - 1 } \
        else if (comparison.aboveZero()) { low := mid + 1 }  \
        else if (comparison.isZero())    { return mid }      \
        else                             { throw("You expect me to binary search with a partial order?") }
    }
    return null
}
