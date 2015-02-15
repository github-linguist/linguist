package require Tcl 8.5
package require struct::prioqueue

proc huffmanEncode {str args} {
    array set opts [concat -dump false $args]

    set charcount [dict create]
    foreach char [split $str ""] {
        dict incr charcount $char
    }

    set pq [struct::prioqueue -dictionary] ;# want lower values to have higher priority
    dict for {char count} $charcount {
        $pq put $char $count
    }

    while {[$pq size] > 1} {
        lassign [$pq peekpriority 2] p1 p2
        $pq put [$pq get 2] [expr {$p1 + $p2}]
    }

    set encoding [walkTree [$pq get]]
    set map [dict create {*}[lreverse $encoding]]

    if {$opts(-dump)} {
        foreach key [lsort -command compare [dict keys $map]] {
            set char [dict get $map $key]
            puts "$char\t[dict get $charcount $char]\t$key"
        }
    }

    return $encoding
}

proc walkTree {tree {prefix ""}} {
    if {[llength $tree] < 2} {
        return [list $tree $prefix]
    }
    lassign $tree left right
    return [concat [walkTree $left "${prefix}0"] [walkTree $right "${prefix}1"]]
}

proc compare {a b} {
    if {[string length $a] < [string length $b]} {return -1}
    if {[string length $a] > [string length $b]} {return  1}
    return [string compare $a $b]
}

set str "this is an example for huffman encoding"

set encoding [huffmanEncode $str -dump true]

puts $str
puts [string map $encoding $str]
