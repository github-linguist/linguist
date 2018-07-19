interp alias {} hanoi {} do_hanoi 0

proc do_hanoi {count n {from A} {to C} {via B}} {
    if {$n == 1} {
        interp alias {} hanoi {} do_hanoi [incr count]
        puts "$count: move from $from to $to"
    } else {
        incr n -1
        hanoi $n $from $via $to
        hanoi 1  $from $to $via
        hanoi $n $via $to $from
    }
}

hanoi 4
