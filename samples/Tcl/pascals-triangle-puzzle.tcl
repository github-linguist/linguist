package require Tcl 8.5
namespace path ::tcl::mathop

set pyramid {
    {151.0 "" "" "" ""}
    {"" "" "" "" ""}
    {40.0 "" "" "" ""}
    {"" "" "" "" ""}
    {x 11.0 y 4.0 z}
}

set equations {{1 -1 1 0}}

proc simplify {terms val} {
    set vars {0 0 0}
    set x 0
    set y 1
    set z 2
    foreach term $terms {
        switch -exact -- $term {
            x - y - z {
                lset vars [set $term] [+ 1 [lindex $vars [set $term]]]
            }
            default {
                set val [- $val $term]
            }
        }
    }
    return [concat $vars $val]
}

for {set row [+ [llength $pyramid] -2]} {$row >= 0} {incr row -1} {
    for {set cell 0} {$cell <= $row} {incr cell } {
	set sum [concat [lindex $pyramid [+ 1 $row] $cell] [lindex $pyramid [+ 1 $row] [+ 1 $cell]]]
	if {[set val [lindex $pyramid $row $cell]] ne ""} {
            lappend equations [simplify $sum $val]
	} else {
            lset pyramid $row $cell  $sum
        }
    }
}

set solution [toRREF $equations]
foreach row $solution {
    lassign $row a b c d
    if {$a + $b + $c > 1} {
        error "problem does not have a unique solution"
    }
    if {$a} {set x $d}
    if {$b} {set y $d}
    if {$c} {set z $d}
}
puts "x=$x"
puts "y=$y"
puts "z=$z"

foreach row $pyramid {
    set newrow {}
    foreach cell $row {
        if {$cell eq ""} {
            lappend newrow ""
        } else {
            lappend newrow [expr [join [string map [list x $x y $y z $z] $cell] +]]
        }
    }
    lappend solved $newrow
}
print_matrix $solved
