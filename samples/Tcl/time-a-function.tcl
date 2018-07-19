proc sum_n {n} {
    for {set i 1; set sum 0.0} {$i <= $n} {incr i} {set sum [expr {$sum + $i}]}
    return [expr {wide($sum)}]
}

puts [time {sum_n 1e6} 100]
puts [time {} 100]
