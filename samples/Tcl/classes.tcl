package require TclOO
oo::class create summation {
    variable v
    constructor {} {
        set v 0
    }
    method add x {
        incr v $x
    }
    method value {} {
        return $v
    }
    destructor {
        puts "Ended with value $v"
    }
}
set sum [summation new]
puts "Start with [$sum value]"
for {set i 1} {$i <= 10} {incr i} {
    puts "Add $i to get [$sum add $i]"
}
$sum destroy
