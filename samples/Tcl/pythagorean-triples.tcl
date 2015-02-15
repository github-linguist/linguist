proc countPythagoreanTriples {limit} {
    lappend q 3 4 5
    set idx [set count [set prim 0]]
    while {$idx < [llength $q]} {
    set a [lindex $q $idx]
    set b [lindex $q [incr idx]]
    set c [lindex $q [incr idx]]
    incr idx
    if {$a + $b + $c <= $limit} {
        incr prim
        for {set i 1} {$i*$a+$i*$b+$i*$c <= $limit} {incr i} {
        incr count
        }
        lappend q \
        [expr {$a + 2*($c-$b)}] [expr {2*($a+$c) - $b}] [expr {2*($a-$b) + 3*$c}] \
        [expr {$a + 2*($b+$c)}] [expr {2*($a+$c) + $b}] [expr {2*($a+$b) + 3*$c}] \
        [expr {2*($b+$c) - $a}] [expr {2*($c-$a) + $b}] [expr {2*($b-$a) + 3*$c}]
    }
    }
    return [list $count $prim]
}
for {set i 10} {$i <= 10000000} {set i [expr {$i*10}]} {
    lassign [countPythagoreanTriples $i] count primitive
    puts "perimeter limit $i => $count triples, $primitive primitive"
}
