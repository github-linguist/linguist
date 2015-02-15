package require TclOO
oo::class create Point {
    variable X Y
    constructor {x y} {
        set X $x
        set Y $y
    }
    method x args {
        set X {*}$args
    }
    method y args {
        set Y {*}$args
    }
    method print {} {
        puts "Point($X,$Y)"
    }
    method copy {} {
        set copy [oo::copy [self]]
        $copy x $X
        $copy y $Y
        return $copy
    }
}
oo::class create Circle {
    superclass Point
    variable R
    constructor {x y radius} {
        next $x $y
        set R $radius
    }
    method radius args {
        set R {*}$args
    }
    method print {} {
        puts "Circle([my x],[my y],$R)"
    }
    method copy {} {
        set copy [next]
        $copy radius $R
        return $copy
    }
}
# No destructors: unneeded by these classes

set p [Point new 1.0 2.0]
set c [Circle new 3.0 4.0 5.0]
set cCopy [$c copy]
puts "$p is at ([$p x],[$p y])"
$c radius 1.5
set objects [list $p $c $cCopy]
foreach o $objects {
    $o print
}
