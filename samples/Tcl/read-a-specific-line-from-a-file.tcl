proc getNthLineFromFile {filename n} {
    set f [open $filename]
    while {[incr n -1] > 0} {
        if {[gets $f line] < 0} {
            close $f
            error "no such line"
        }
    }
    close $f
    return $line
}

puts [getNthLineFromFile example.txt 7]
