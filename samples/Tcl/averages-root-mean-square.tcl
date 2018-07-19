proc qmean list {
    set sum 0.0
    foreach value $list { set sum [expr {$sum + $value**2}] }
    return [expr { sqrt($sum / [llength $list]) }]
}

puts "RMS(1..10) = [qmean {1 2 3 4 5 6 7 8 9 10}]"
