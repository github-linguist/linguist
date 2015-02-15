package require Tcl 8.5
package require struct::list

proc inorder {list} {::tcl::mathop::<= {*}$list}

proc permutationsort {list} {
    while { ! [inorder $list]} {
        set list [struct::list nextperm $list]
    }
    return $list
}
