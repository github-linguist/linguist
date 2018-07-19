List do(
    combSortInPlace := method(
        gap := size
        swap := true

        while(gap > 1 or swap,
            swap = false
            gap = (gap / 1.25) floor

            for(i, 0, size - gap,
                if(at(i) > at(i + gap),
                    swap = true
                    swapIndices(i, i + gap)
                )
            )
        )
    self)
)

lst := list(23, 76, 99, 58, 97, 57, 35, 89, 51, 38, 95, 92, 24, 46, 31, 24, 14, 12, 57, 78)
lst combSortInPlace println # ==> list(12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99)
