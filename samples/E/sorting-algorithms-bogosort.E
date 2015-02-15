def isSorted(list) {
    if (list.size() == 0) { return true }
    var a := list[0]
    for i in 1..!(list.size()) {
        var b := list[i]
        if (a > b) { return false }
        a := b
    }
    return true
}

def bogosort(list, random) {
    while (!isSorted(list)) {
        shuffle(list, random)
    }
}
