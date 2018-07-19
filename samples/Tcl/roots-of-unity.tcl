package require Tcl 8.5
namespace import tcl::mathfunc::*

set pi 3.14159265
for {set n 2} {$n <= 10} {incr n} {
    set angle 0.0
    set row $n:
    for {set i 1} {$i <= $n} {incr i} {
        lappend row [format %5.4f%+5.4fi [cos $angle] [sin $angle]]
        set angle [expr {$angle + 2*$pi/$n}]
    }
    puts $row
}
