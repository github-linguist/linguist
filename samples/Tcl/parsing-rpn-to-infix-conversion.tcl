package require Tcl 8.5

# Helpers
proc precassoc op {
    dict get {^ {4 right} * {3 left} / {3 left} + {2 left} - {2 left}} $op
}
proc pop stk {
    upvar 1 $stk s
    set val [lindex $s end]
    set s [lreplace $s end end]
    return $val
}

proc rpn2infix rpn {
    foreach token $rpn {
	switch $token {
	    "^" - "/" - "*" - "+" - "-" {
		lassign [pop stack] bprec b
		lassign [pop stack] aprec a
		lassign [precassoc $token] p assoc
		if {$aprec < $p || ($aprec == $p && $assoc eq "right")} {
		    set a "($a)"
		}
		if {$bprec < $p || ($bprec == $p && $assoc eq "left")} {
		    set b "($b)"
		}
		lappend stack [list $p "$a $token $b"]
	    }
	    default {
		lappend stack [list 9 $token]
	    }
	}
	puts "$token -> $stack"
    }
    return [lindex $stack end 1]
}

puts [rpn2infix {3 4 2 * 1 5 - 2 3 ^ ^ / +}]
puts [rpn2infix {1 2 + 3 4 + ^ 5 6 + ^}]
