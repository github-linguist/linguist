package require Tcl 8.5

set f {n {expr {$n + floor(0.5 + sqrt($n))}}}

for {set x 1} {$x <= 22} {incr x} {
    puts [format "%d\t%s" $x [apply $f $x]]
}

puts "looking for a square..."
for {set x 1} {$x <= 1000000} {incr x} {
    set y [apply $f $x]
    set s [expr {sqrt($y)}]
    if {$s == int($s)} {
        error "found a square in the sequence: $x -> $y"
    }
}
puts "done"
