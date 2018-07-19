fun oneHundredDoors(): List<Int> {
    val doors = Array<Boolean>(100, { false })

    for (i in 0..99)
        for (j in i..99 step (i + 1))
            doors[j] = !doors[j]

    return IndexIterator(doors.iterator()).filter { it.second }
                                          .map { it.first + 1 }
                                          .toList()
}
