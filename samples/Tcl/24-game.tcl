# Four random non-zero digits
proc choose4 {} {
    set digits {}
    foreach x {1 2 3 4} {lappend digits [expr {int(1+rand()*9)}]}
    return [lsort $digits]
}

# Print out a welcome message
proc welcome digits {
    puts [string trim "
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
brackets, (), show how to make an answer of 24.

An answer of \"q\" will quit the game.
An answer of \"!\" will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.
    "]
    puts "\nYour four digits: $digits"
}

# Check whether we've got a legal answer
proc check {answer digits} {
    if {
	[regexp "\[^-+*/() \t[join $digits {}]\]" $answer]
	|| [regexp {\d\d} $answer]
    } then {
	return false
    }
    set digs [lsort [regexp -inline -all {\d} $answer]]
    if {$digs ne $digits} {
	return false
    }
    expr {![catch {expr $answer}]}
}

# The main game loop
proc main {} {
    fconfigure stdout -buffering none

    set digits [choose4]
    welcome $digits
    set trial 0
    while true {
	puts -nonewline "Expression [incr trial]: "
	gets stdin answer

        # Check for various types of non-answer
	if {[eof stdin] || $answer eq "q" || $answer eq "Q"} {
	    break
	} elseif {$answer eq "!"} {
	    set digits [choose4]
	    puts "New digits: $digits"
	    continue
	} elseif {![check $answer $digits]} {
	    puts "The input '$answer' was wonky!"
            continue
	}

        # Check to see if it is the right answer
	set ans [expr [regsub {\d} $answer {&.0}]]
	puts " = [string trimright $ans .0]"
	if {$ans == 24.0} {
	    puts "That's right!"
            break
	}
    }
    puts "Thank you and goodbye"
}
main
