proc randelem {list} {
    lindex $list [expr {int(rand()*[llength $list])}]
}
set x [randelem {1 2 3 4 5}]
