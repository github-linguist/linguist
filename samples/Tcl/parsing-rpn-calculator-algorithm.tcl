# Helper
proc pop stk {
    upvar 1 $stk s
    set val [lindex $s end]
    set s [lreplace $s end end]
    return $val
}

proc evaluate rpn {
    set stack {}
    foreach token $rpn {
	set act "apply"
	switch $token {
	    "^" {
		# Non-commutative operation
		set a [pop stack]
		lappend stack [expr {[pop stack] ** $a}]
	    }
	    "/" {
		# Non-commutative, special float handling
		set a [pop stack]
		set b [expr {[pop stack] / double($a)}]
		if {$b == round($b)} {set b [expr {round($b)}]}
		lappend stack $b
	    }
	    "*" {
		# Commutative operation
		lappend stack [expr {[pop stack] * [pop stack]}]
	    }
	    "-" {
		# Non-commutative operation
		set a [pop stack]
		lappend stack [expr {[pop stack] - $a}]
	    }
	    "+" {
		# Commutative operation
		lappend stack [expr {[pop stack] + [pop stack]}]
	    }
	    default {
		set act "push"
		lappend stack $token
	    }
	}
	puts "$token\t$act\t$stack"
    }
    return [lindex $stack end]
}

puts [evaluate {3 4 2 * 1 5 - 2 3 ^ ^ / +}]
