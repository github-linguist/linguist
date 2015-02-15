package require Tcl 8.5

### Choices are represented by integers, which are indices into this list:
###    Rock, Paper, Scissors
### Normally, idiomatic Tcl code uses names for these sorts of things, but it
### turns out that using integers simplifies the move-comparison logic.

# How to ask for a move from the human player
proc getHumanMove {} {
    while 1 {
	puts -nonewline "Your move? \[R\]ock, \[P\]aper, \[S\]cissors: "
	flush stdout
	gets stdin line
	if {[eof stdin]} {
	    puts "\nBye!"
	    exit
	}
	set len [string length $line]
	foreach play {0 1 2} name {"rock" "paper" "scissors"} {
	    # Do a prefix comparison
	    if {$len && [string equal -nocase -length $len $line $name]} {
		return $play
	    }
	}
	puts "Sorry, I don't understand that. Try again please."
    }
}

# How to ask for a move from the machine player
proc getMachineMove {} {
    global states
    set choice [expr {int(rand() * [::tcl::mathop::+ {*}$states 3])}]
    foreach play {1 2 0} count $states {
	if {[incr sum [expr {$count+1}]] > $choice} {
	    puts "I play \"[lindex {Rock Paper Scissors} $play]\""
	    return $play
	}
    }
}

# Initialize some state variables
set states {0 0 0}
set humanWins 0
set machineWins 0

# The main game loop
while 1 {
    # Get the moves for this round
    set machineMove [getMachineMove]
    set humanMove [getHumanMove]
    # Report on what happened
    if {$humanMove == $machineMove} {
	puts "A draw!"
    } elseif {($humanMove+1)%3 == $machineMove} {
	puts "I win!"
	incr machineWins
    } else {
	puts "You win!"
	incr humanWins
    }
    puts "Cumulative scores: $humanWins to you, $machineWins to me"
    # Update the state of how the human has played in the past
    lset states $humanMove [expr {[lindex $states $humanMove] + 1}]
}
