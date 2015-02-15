fun <T> bubbleSort(a : Array<T>, c: Comparator<T>) {
    var changed : Boolean
    do {
        changed = false
        for (i in 0 .. a.size - 2) {
            if (c.compare(a[i], a[i + 1]) > 0) {
                val tmp = a[i]
                a[i] = a[i + 1]
                a[i + 1] = tmp
                changed = true
            }
        }
    } while (changed)
}
