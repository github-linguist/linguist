package require Tcl 8.5

# Helpers
proc tokenize {str} {
    regexp -all -inline {[\d.]+|[-*^+/()]} $str
}
proc precedence op {
    dict get {^ 4 * 3 / 3 + 2 - 2} $op
}
proc associativity op {
    if {$op eq "^"} {return "right"} else {return "left"}
}

proc shunting {expression} {
    set stack {}
    foreach token [tokenize $expression] {
	if {[string is double $token]} {
	    puts "add to output: $token"
	    lappend output $token
	} elseif {$token eq "("} {
	    puts "push parenthesis"
	    lappend stack $token
	} elseif {$token eq ")"} {
	    puts "popping to parenthesis"
	    while {[lindex $stack end] ne "("} {
		lappend output [lindex $stack end]
		set stack [lreplace $stack end end]
		puts "...popped [lindex $output end] to output"
	    }
	    set stack [lreplace $stack end end]
	    puts "...found parenthesis"
	} else {
	    puts "adding operator: $token"
	    set p [precedence $token]
	    set a [associativity $token]
	    while {[llength $stack]} {
		set o2 [lindex $stack end]
		if {
		    $o2 ne "(" &&
		    (($a eq "left" && $p <= [precedence $o2]) ||
		    ($a eq "right" && $p < [precedence $o2]))
		} then {
		    puts "...popped operator $o2 to output"
		    lappend output $o2
		    set stack [lreplace $stack end end]
		} else {
		    break
		}
	    }
	    lappend stack $token
	}
	puts "\t\tOutput:\t$output\n\t\tStack:\t$stack"
    }
    puts "transferring tokens from stack to output"
    lappend output {*}[lreverse $stack]
}

puts [shunting "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"]
