set ary [subst [lrepeat 10 [lrepeat 5 {[expr int(rand()*20+1)]}]]]

try {
    foreach row $ary {
        foreach col $row {
            puts -nonewline [format %3s $col]
            if {$col == 20} {
                throw MULTIBREAK "we're done"
            }
        }
        puts ,
    }
} trap MULTIBREAK {} {}
puts " done"
