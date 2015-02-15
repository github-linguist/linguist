while true {
    set a [expr int(20*rand())]
    puts $a
    if {$a == 10} {
        break
    }
    set b [expr int(20*rand())]
    puts $b
}
