package require Tcl 8.5
proc pop {varname} {
    upvar 1 $varname var
    set var [lassign $var head]
    return $head
}

proc common_prefix {dirs {separator "/"}} {
    set parts [split [pop dirs] $separator]
    while {[llength $dirs]} {
        set r {}
        foreach cmp $parts elt [split [pop dirs] $separator] {
            if {$cmp ne $elt} break
            lappend r $cmp
        }
        set parts $r
    }
    return [join $parts $separator]
}
