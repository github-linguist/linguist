def quicksort := {

    def swap(container, ixA, ixB) {
        def temp := container[ixA]
        container[ixA] := container[ixB]
        container[ixB] := temp
    }

    def partition(array, var first :int, var last :int) {
        if (last <= first) { return }

        # Choose a pivot
        def pivot := array[def pivotIndex := (first + last) // 2]

        # Move pivot to end temporarily
        swap(array, pivotIndex, last)

        var swapWith := first

        # Scan array except for pivot, and...
        for i in first..!last {
            if (array[i] <= pivot) {   # items ≤ the pivot
                swap(array, i, swapWith) # are moved to consecutive positions on the left
                swapWith += 1
            }
        }

        # Swap pivot into between-partition position.
        # Because of the swapping we know that everything before swapWith is less
        # than or equal to the pivot, and the item at swapWith (since it was not
        # swapped) is greater than the pivot, so inserting the pivot at swapWith
        # will preserve the partition.
        swap(array, swapWith, last)
        return swapWith
    }

    def quicksortR(array, first :int, last :int) {
        if (last <= first) { return }
        def pivot := partition(array, first, last)
        quicksortR(array, first, pivot - 1)
        quicksortR(array, pivot + 1, last)
    }

    def quicksort(array) { # returned from block
        quicksortR(array, 0, array.size() - 1)
    }
}
