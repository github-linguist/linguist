List do (
    cocktailSortInPlace := method(
        start := 0
        end := size - 2

        loop(
            swapped := false

            for(idx, start, end,
                if(at(idx) > at(idx + 1),
                    swapped := true
                    swapIndices(idx, idx + 1)
                )
            )

            if(swapped not, break, end := end - 1)

            for (idx, end, start, -1,
                if(at(idx) > at(idx + 1),
                    swapped := true
                    swapIndices(idx, idx + 1)
                )
            )

            if(swapped not, break, start := start + 1)
        )
    self)
)

l := list(2, 3, 4, 5, 1)
l cocktailSortInPlace println # ==> list(1, 2, 3, 4, 5)
