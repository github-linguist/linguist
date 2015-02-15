proc factors {n} {
    set factors {}
    for {set i 1} {$i <= sqrt($n)} {incr i} {
        if {$n % $i == 0} {
            lappend factors $i [expr {$n / $i}]
        }
    }
    return [lsort -unique -integer $factors]
}
puts [factors 64]
puts [factors 45]
puts [factors 53]
