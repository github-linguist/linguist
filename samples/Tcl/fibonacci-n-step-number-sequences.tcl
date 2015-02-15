package require Tcl 8.6

proc fibber {args} {
    coroutine fib[incr ::fibs]=[join $args ","] apply {fn {
	set n [info coroutine]
	foreach f $fn {
	    if {![yield $n]} return
	    set n $f
	}
	while {[yield $n]} {
	    set fn [linsert [lreplace $fn 0 0] end [set n [+ {*}$fn]]]
	}
    } ::tcl::mathop} $args
}

proc print10 cr {
    for {set i 1} {$i <= 10} {incr i} {
	lappend out [$cr true]
    }
    puts \[[join [lappend out ...] ", "]\]
    $cr false
}
puts "FIBONACCI"
print10 [fibber 1 1]
puts "TRIBONACCI"
print10 [fibber 1 1 2]
puts "TETRANACCI"
print10 [fibber 1 1 2 4]
puts "LUCAS"
print10 [fibber 2 1]
