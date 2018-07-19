proc is_happy n {
    set seen [list]
    while {$n > 1 && [lsearch -exact $seen $n] == -1} {
        lappend seen $n
        set n [sum_of_squares [split $n ""]]
    }
    return [expr {$n == 1}]
}

set happy [list]
set n -1
while {[llength $happy] < 8} {
    if {[is_happy $n]} {lappend happy $n}
    incr n
}
puts "the first 8 happy numbers are: [list $happy]"
