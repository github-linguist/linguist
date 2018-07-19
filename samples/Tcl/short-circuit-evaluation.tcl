package require Tcl 8.5
proc tcl::mathfunc::a boolean {
    puts "a($boolean) called"
    return $boolean
}
proc tcl::mathfunc::b boolean {
    puts "b($boolean) called"
    return $boolean
}

foreach i {false true} {
    foreach j {false true} {
        set x [expr {a($i) && b($j)}]
        puts "x = a($i) && b($j) = $x"
        set y [expr {a($i) || b($j)}]
        puts "y = a($i) || b($j) = $y"
        puts ""; # Blank line for clarity
    }
}
