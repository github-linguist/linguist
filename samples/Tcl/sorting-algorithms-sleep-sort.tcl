#!/bin/env tclsh
set count 0
proc process val {
    puts $val
    incr ::count
}
# Schedule the output of the values
foreach val $argv {
    after [expr {$val * 10}] [list process $val]
}
# Run event loop until all values output...
while {$count < $argc} {
    vwait count
}
