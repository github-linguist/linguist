package require Tcl 8.6

# This code splits the players from the core game engine
oo::class create TicTacToe {
    variable board player letter who
    constructor {player1class player2class} {
	set board {1 2 3 4 5 6 7 8 9}
	set player(0) [$player1class new [self] [set letter(0) "X"]]
	set player(1) [$player2class new [self] [set letter(1) "O"]]
	set who 0
    }

    method PrintBoard {} {
	lassign $board a1 b1 c1 a2 b2 c2 a3 b3 c3
	puts [format " %s | %s | %s" $a1 $b1 $c1]
	puts "---+---+---"
	puts [format " %s | %s | %s" $a2 $b2 $c2]
	puts "---+---+---"
	puts [format " %s | %s | %s" $a3 $b3 $c3]
    }

    method WinForSomeone {} {
	foreach w {
	    {0 1 2} {3 4 5} {6 7 8} {0 3 6} {1 4 7} {2 5 8} {0 4 8} {2 4 6}
	} {
	    set b [lindex $board [lindex $w 0]]
	    if {$b ni "X O"} continue
	    foreach i $w {if {[lindex $board $i] ne $b} break}
	    if {[lindex $board $i] eq $b} {
		foreach p $w {lappend w1 [expr {$p+1}]}
		return [list $b $w1]
	    }
	}
	return ""
    }

    method status {} {
	return $board
    }

    method IsDraw {} {
	foreach b $board {if {[string is digit $b]} {return false}}
	return true
    }

    method legalMoves {} {
	foreach b $board {if {[string is digit $b]} {lappend legal $b}}
	return $legal
    }

    method DoATurn {} {
	set legal [my legalMoves]
	my PrintBoard
	while 1 {
	    set move [$player($who) turn]
	    if {$move in $legal} break
	    puts "Illegal move!"
	}
	lset board [expr {$move - 1}] $letter($who)
	$player($who) describeMove $move
	set who [expr {1 - $who}]
	return [my WinForSomeone]
    }

    method game {} {
        puts "    Tic-tac-toe game player.
    Input the index of where you wish to place your mark at your turn.\n"
	while {![my IsDraw]} {
	    set winner [my DoATurn]
	    if {$winner eq ""} continue
	    lassign $winner winLetter winSites
	    my PrintBoard
	    puts "\n$winLetter wins across \[[join $winSites {, }]\]"
	    return $winLetter
	}
	puts "\nA draw"
    }
}

# Stupid robotic player
oo::class create RandomRoboPlayer {
    variable g
    constructor {game letter} {
	set g $game
    }
    method turn {} {
	set legal [$g legalMoves]
	return [lindex $legal [expr {int(rand()*[llength $legal])}]]
    }
    method describeMove {move} {
	puts "I go at $move"
    }
}

# Interactive human player delegate
oo::class create HumanPlayer {
    variable g char
    constructor {game letter} {
	set g $game
	set char $letter
    }
    method turn {} {
	set legal [$g legalMoves]
	puts ">>> Put your $char in any of these positions: [join $legal {}]"
	while 1 {
	    puts -nonewline ">>> "
	    flush stdout
	    gets stdin number
	    if {$number in $legal} break
	    puts ">>> Whoops I don't understand the input!"
	}
	return $number
    }
    method describeMove {move} {
	puts "You went at $move"
    }
}

# Assemble the pieces
set ttt [TicTacToe new HumanPlayer RandomRoboPlayer]
$ttt game
