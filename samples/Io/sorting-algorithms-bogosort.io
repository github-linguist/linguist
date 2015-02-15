List do(
    isSorted := method(
        slice(1) foreach(i, x,
            if (x < at(i), return false)
        )
        return true;
    )

    bogoSortInPlace := method(
        while(isSorted not,
            shuffleInPlace()
        )
    )
)

lst := list(2, 1, 4, 3)
lst bogoSortInPlace println # ==> list(1, 2, 3, 4), hopefully :)
