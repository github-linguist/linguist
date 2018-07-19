proc is_prime n {
    if {$n <= 1} {return false}
    if {$n == 2} {return true}
    if {$n % 2 == 0} {return false}
    for {set i 3} {$i <= sqrt($n)} {incr i 2} {
        if {$n % $i == 0} {return false}
    }
    return true
}
