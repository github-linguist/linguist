proc D5 {} {expr {1 + int(5 * rand())}}

proc D7 {} {
    while 1 {
        set d55 [expr {5 * [D5] + [D5] - 6}]
        if {$d55 < 21} {
            return [expr {$d55 % 7 + 1}]
        }
    }
}
