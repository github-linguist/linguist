proc m {n} {
    if { $n == 0 } { expr 0; } else {
	expr {$n - [f [m [expr {$n-1}] ]]};
    }
}
proc f {n} {
    if { $n == 0 } { expr 1; } else {
	expr {$n - [m [f [expr {$n-1}] ]]};
    }
}

for {set i 0} {$i < 20} {incr i} {
    puts -nonewline [f $i];
    puts -nonewline " ";
}
puts ""
for {set i 0} {$i < 20} {incr i} {
    puts -nonewline [m $i];
    puts -nonewline " ";
}
puts ""
