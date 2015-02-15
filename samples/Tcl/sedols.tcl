namespace eval sedol {
    variable chars {0 1 2 3 4 5 6 7 8 9 "" B C D "" F G H "" J K L M N "" P Q R S T "" V W X Y Z}
    variable weight {1 3 1 7 3 9 1}

    proc checksum {alnum6} {
        variable chars
        variable weight
        set sum 0
        set col 0
        foreach char [split [string toupper [string range $alnum6 0 5]] ""] {
            if {[set idx [lsearch -exact $chars $char]] == -1} {
                error "invalid character: $char"
            }
            incr sum [expr {$idx * [lindex $weight $col]}]
            incr col
        }
        return [expr {(10 - ($sum % 10)) % 10}]
    }

    proc valid {alnum7} {
        expr {[checksum [string range $alnum7 0 5]] == [string index $alnum7 6]}
    }
}

proc assert {condition {message "Assertion failed!"}} {
    if { ! [uplevel 1 [list expr $condition]]} {
        return -code error $message
    }
}

set codes {710889 B0YBKJ 406566 B0YBLH 228276 B0YBKL 557910 B0YBKR 585284 B0YBKT}
set answers {7108899 B0YBKJ7 4065663 B0YBLH2 2282765 B0YBKL9 5579107 B0YBKR5 5852842 B0YBKT7}

foreach code $codes answer $answers {
    set sedol "${code}[sedol::checksum $code]"
    assert {$sedol eq $answer} "assertion failed: $sedol ne $answer"
    puts $sedol
}
