proc pi {samples} {
    set i 0
    set inside 0
    while {[incr i] <= $samples} {
        if {sqrt(rand()**2 + rand()**2) <= 1.0} {
            incr inside
        }
    }
    return [expr {4.0 * $inside / $samples}]
}

puts "PI is approx [expr {atan(1)*4}]\n"
foreach runs {1e2 1e4 1e6 1e8} {
    puts "$runs => [pi $runs]"
}
