List do (
    selectionSortInPlace := method(
        size repeat(idx,
            swapIndices(idx, indexOf(slice(idx, size) min))
        )
    )
)

l := list(-1, 4, 2, -9)
l selectionSortInPlace println # ==> list(-9, -1, 2, 4)
