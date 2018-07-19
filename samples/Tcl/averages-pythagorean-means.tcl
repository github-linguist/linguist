proc arithmeticMean list {
    set sum 0.0
    foreach value $list { set sum [expr {$sum + $value}] }
    return [expr {$sum / [llength $list]}]
}
proc geometricMean list {
    set product 1.0
    foreach value $list { set product [expr {$product * $value}] }
    return [expr {$product ** (1.0/[llength $list])}]
}
proc harmonicMean list {
    set sum 0.0
    foreach value $list { set sum [expr {$sum + 1.0/$value}] }
    return [expr {[llength $list] / $sum}]
}

set nums {1 2 3 4 5 6 7 8 9 10}
set A10 [arithmeticMean $nums]
set G10 [geometricMean $nums]
set H10 [harmonicMean $nums]
puts "A10=$A10, G10=$G10, H10=$H10"
if {$A10 >= $G10} { puts "A10 >= G10" }
if {$G10 >= $H10} { puts "G10 >= H10" }
