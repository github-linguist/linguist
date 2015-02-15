proc agm {a b} {
    set old_b [expr {$b<0?inf:-inf}]
    while {$a != $b && $b != $old_b} {
	set old_b $b
	lassign [list [expr {0.5*($a+$b)}] [expr {sqrt($a*$b)}]] a b
    }
    return $a
}

puts [agm 1 [expr 1/sqrt(2)]]
