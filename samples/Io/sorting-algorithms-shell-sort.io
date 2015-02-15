List do(
    shellSortInPlace := method(
        gap := (size / 2) round
        while(gap > 0,
            for(i, gap, size - 1,
                key := at(i)
                j := i

                while(j >= gap and at(j - gap) > key,
                    atPut(j, at(j - gap))
                    j = j - gap
                )
                atPut(j, key)
            )
            gap = (gap / 2.2) round
        )
    self)
)

l := list(2, 3, 4, 5, 1)
l shellSortInPlace println # ==> list(1, 2, 3, 4, 5)
