package require struct::list

# Make the sequence of digits to be permuted
set n [lindex $argv 0]
for {set i 1} {$i <= $n} {incr i} {lappend sequence $i}

# Iterate over the permutations, printing as we go
struct::list foreachperm p $sequence {
    puts $p
}
