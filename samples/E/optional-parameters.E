def defaultOrdering(a, b) { return a.op__cmp(b) }

def sort {

    to run(table) {
        return sort(table, 0, false, defaultOrdering)
    }
    to run(table, column) {
        return sort(table, column, false, defaultOrdering)
    }
    to run(table, column, reverse) {
        return sort(table, column, reverse, defaultOrdering)
    }

    to run(table :List[List[String]], column :int, reverse :boolean, ordering) {
        return table.sort(fn a, b {
            def ord := ordering(a[column], b[column])
            if (reverse) { -ord } else { ord }
        })
    }

}
