List do (
    merge := method(lst1, lst2,
        result := list()
        while(lst1 isNotEmpty or lst2 isNotEmpty,
            if(lst1 first <= lst2 first) then(
                result append(lst1 removeFirst)
            ) else (
                result append(lst2 removeFirst)
            )
        )
    result)

    mergeSort := method(
        if (size > 1) then(
            half_size := (size / 2) ceil
            return merge(slice(0, half_size) mergeSort,
                         slice(half_size, size) mergeSort)
        ) else (return self)
    )

    mergeSortInPlace := method(
        copy(mergeSort)
    )
)

lst := list(9, 5, 3, -1, 15, -2)
lst mergeSort println # ==> list(-2, -1, 3, 5, 9, 15)
lst mergeSortInPlace println # ==> list(-2, -1, 3, 5, 9, 15)
