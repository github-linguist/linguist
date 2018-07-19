package require Tcl 8.5
# Simple shuffler, not very efficient but good enough for here
proc shuffle list {
    set result {}
    while {[llength $list]} {
	set i [expr {int([llength $list] * rand())}]
	lappend result [lindex $list $i]
	set list [lreplace $list $i $i]
    }
    return $result
}
# Returns the list with the prefix of it reversed
proc flipfirst {list n} {
    concat [lreverse [lrange $list 0 $n-1]] [lrange $list $n end]
}

# Core game engine; list to play with is optional argument
proc nrgame {{target "1 2 3 4 5 6 7 8 9"}} {
    set nums $target
    while {$nums eq $target} {set nums [shuffle $nums]}
    set goes 0
    while {$nums ne $target} {
	incr goes
	puts -nonewline "#${goes}: List is '[join $nums {, }]', how many to reverse? "
	flush stdout
	gets stdin n
	if {$n eq "q"} {return quit}
	# Input validation would go here
	set nums [flipfirst $nums $n]
    }
    return $goes
}

# Print some instructions and wait for the user to win
puts "Welcome to the Number Reversal Game!"
puts "------------------------------------"
puts "I'll show you a list of numbers, you need to reverse prefixes of them"
puts "to get the whole list in ascending order. A 'q' will quit early.\n"
puts ""
set outcome [nrgame]
if {$outcome ne "quit"} {
    puts "\nYou took $outcome attempts to put the digits in order."
}
