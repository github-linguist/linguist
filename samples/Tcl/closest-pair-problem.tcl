package require Tcl 8.5

# retrieve the x-coordinate
proc x p {lindex $p 0}
# retrieve the y-coordinate
proc y p {lindex $p 1}

proc distance {p1 p2} {
    expr {hypot(([x $p1]-[x $p2]), ([y $p1]-[y $p2]))}
}

proc closest_bruteforce {points} {
    set n [llength $points]
    set mindist Inf
    set minpts {}
    for {set i 0} {$i < $n - 1} {incr i} {
        for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
            set p1 [lindex $points $i]
            set p2 [lindex $points $j]
            set dist [distance $p1 $p2]
            if {$dist < $mindist} {
                set mindist $dist
                set minpts [list $p1 $p2]
            }
        }
    }
    return [list $mindist $minpts]
}

proc closest_recursive {points} {
    set n [llength $points]
    if {$n <= 3} {
        return [closest_bruteforce $points]
    }
    set xP [lsort -real -increasing -index 0 $points]
    set mid [expr {int(ceil($n/2.0))}]
    set PL [lrange $xP 0 [expr {$mid-1}]]
    set PR [lrange $xP $mid end]
    set procname [lindex [info level 0] 0]
    lassign [$procname $PL] dL pairL
    lassign [$procname $PR] dR pairR
    if {$dL < $dR} {
        set dmin $dL
        set dpair $pairL
    } else {
        set dmin $dR
        set dpair $pairR
    }

    set xM [x [lindex $PL end]]
    foreach p $xP {
        if {abs($xM - [x $p]) < $dmin} {
            lappend S $p
        }
    }
    set yP [lsort -real -increasing -index 1 $S]
    set closest Inf
    set nP [llength $yP]
    for {set i 0} {$i <= $nP-2} {incr i} {
        set yPi [lindex $yP $i]
        for {set k [expr {$i+1}]; set yPk [lindex $yP $k]} {
            $k < $nP-1 && ([y $yPk]-[y $yPi]) < $dmin
        } {incr k; set yPk [lindex $yP $k]} {
            set dist [distance $yPk $yPi]
            if {$dist < $closest} {
                set closest $dist
                set closestPair [list $yPi $yPk]
            }
        }
    }
    expr {$closest < $dmin ? [list $closest $closestPair] : [list $dmin $dpair]}
}

# testing
set N 10000
for {set i 1} {$i <= $N} {incr i} {
    lappend points [list [expr {rand()*100}] [expr {rand()*100}]]
}

# instrument the number of calls to [distance] to examine the
# efficiency of the recursive solution
trace add execution distance enter comparisons
proc comparisons args {incr ::comparisons}

puts [format "%-10s  %9s  %9s  %s" method compares time closest]
foreach method {bruteforce recursive} {
    set ::comparisons 0
    set time [time {set ::dist($method) [closest_$method $points]} 1]
    puts [format "%-10s  %9d  %9d  %s" $method $::comparisons [lindex $time 0] [lindex $::dist($method) 0]]
}
