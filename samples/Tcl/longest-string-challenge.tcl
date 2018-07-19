#!/usr/bin/env tclsh

set longest z
set output ""
while {[gets stdin line] != -1} {
    set comparison [string repeat z [string length $line]]
    if {$longest eq $comparison} {
        # this line is equally long
        append output $line \n
    } elseif {[string match ${longest}z* $comparison]} {
        # this line is longer
        set longest $comparison
        set output "$line\n"
    }
}
puts -nonewline $output
