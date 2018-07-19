def merge(var xs :List, var ys :List) {
    var result := []
    while (xs =~ [x] + xr && ys =~ [y] + yr) {
        if (x <= y) {
            result with= x
            xs := xr
        } else {
            result with= y
            ys := yr
        }
    }
    return result + xs + ys
}

def sort(list :List) {
    if (list.size() <= 1) { return list }
    def split := list.size() // 2
    return merge(sort(list.run(0, split)),
                 sort(list.run(split)))
}
