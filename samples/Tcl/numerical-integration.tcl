package require Tcl 8.5

proc leftrect {f left right} {
    $f $left
}
proc midrect {f left right} {
    set mid [expr {($left + $right) / 2.0}]
    $f $mid
}
proc rightrect {f left right} {
    $f $right
}
proc trapezium {f left right} {
    expr {([$f $left] + [$f $right]) / 2.0}
}
proc simpson {f left right} {
    set mid [expr {($left + $right) / 2.0}]
    expr {([$f $left] + 4*[$f $mid] + [$f $right]) / 6.0}
}

proc integrate {f a b steps method} {
    set delta [expr {1.0 * ($b - $a) / $steps}]
    set total 0.0
    for {set i 0} {$i < $steps} {incr i} {
        set left [expr {$a + $i * $delta}]
        set right [expr {$left + $delta}]
        set total [expr {$total + $delta * [$method $f $left $right]}]
    }
    return $total
}

interp alias {} sin {} ::tcl::mathfunc::sin
proc square x {expr {$x*$x}}
proc def_int {f a b} {
    switch -- $f {
        sin    {set lambda {x {expr {-cos($x)}}}}
        square {set lambda {x {expr {$x**3/3.0}}}}
    }
    return [expr {[apply $lambda $b] - [apply $lambda $a]}]
}

set a 0
set b [expr {4*atan(1)}]
set steps 10

foreach func {square sin} {
    puts "integral of ${func}(x) from $a to $b in $steps steps"
    set actual [def_int $func $a $b]
    foreach method {leftrect midrect rightrect trapezium simpson} {
        set int [integrate $func $a $b $steps $method]
        set diff [expr {($int - $actual) * 100.0 / $actual}]
        puts [format "   %-10s  %s\t(%.1f%%)" $method $int $diff]
    }
}
