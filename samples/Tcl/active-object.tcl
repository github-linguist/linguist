package require Tcl 8.6
oo::class create integrator {
    variable e sum delay tBase t0 k0 aid
    constructor {{interval 1}} {
	set delay $interval
	set tBase [clock microseconds]
	set t0 0
	set e { 0.0 }
	set k0 0.0
	set sum 0.0
	set aid [after $delay [namespace code {my Step}]]
    }
    destructor {
	after cancel $aid
    }
    method input expression {
	set e $expression
    }
    method output {} {
	return $sum
    }
    method Eval t {
	expr $e
    }
    method Step {} {
	set aid [after $delay [namespace code {my Step}]]
	set t [expr {([clock microseconds] - $tBase) / 1e6}]
	set k1 [my Eval $t]
	set sum [expr {$sum + ($k1 + $k0) * ($t - $t0) / 2.}]
	set t0 $t
	set k0 $k1
    }
}

set pi 3.14159265
proc pause {time} {
    yield [after [expr {int($time * 1000)}] [info coroutine]]
}
proc task {script} {
    coroutine task_ apply [list {} "$script;set ::done ok"]
    vwait done
}
task {
    integrator create i
    i input {sin(2*$::pi * 0.5 * $t)}
    pause 2
    i input { 0.0 }
    pause 0.5
    puts [format %.15f [i output]]
}
