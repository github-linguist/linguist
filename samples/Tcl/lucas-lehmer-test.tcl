proc main argv {
    set n 0
    set t [clock seconds]
    show_mersenne 2 [incr n] t

    for {set p 3} {$p <= [lindex $argv 0]} {incr p 2} {
        if {![prime $p]} continue
        if {[LucasLehmer $p]} {
            show_mersenne $p [incr n] t
        }
    }
}
proc show_mersenne {p n timevar} {
    upvar 1 $timevar time
    set now [clock seconds]
    puts [format "%2d: %5ds  M%s" $n [expr {$now - $time}] $p]
    set time $now
}
proc prime i {
   if {$i in {2 3}} {return 1}
   prime0 $i [expr {int(sqrt($i))}]
}
proc prime0 {i div} {
    expr {!($i % $div)? 0: $div <= 2? 1: [prime0 $i [incr div -1]]}
}
proc LucasLehmer p {
    set mp [expr {2**$p-1}]
    set s  4
    for {set i 2} {$i < $p} {incr i} {
        set s [expr {($s**2 - 2) % $mp}]
    }
    expr {$s == 0}
}

main 33218
