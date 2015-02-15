package require struct::list
package require struct::set

proc scorecalc {guess chosen} {
    set bulls 0
    set cows 0
    foreach g $guess c $chosen {
	if {$g eq $c} {
	    incr bulls
	} elseif {$g in $chosen} {
	    incr cows
	}
    }
    return [list $bulls $cows]
}

# Allow override on command line
set size [expr {$argc ? int($argv) : 4}]

set choices {}
struct::list foreachperm p [split 123456789 ""] {
    struct::set include choices [lrange $p 1 $size]
}
set answers {}
set scores {}

puts "Playing Bulls & Cows with $size unique digits\n"
fconfigure stdout -buffering none
while 1 {
    set ans [lindex $choices [expr {int(rand()*[llength $choices])}]]
    lappend answers $ans
    puts -nonewline \
	"Guess [llength $answers] is [join $ans {}]. Answer (Bulls, cows)? "
    set score [scan [gets stdin] %d,%d]
    lappend scores $score
    if {$score eq {$size 0}} {
	puts "Ye-haw!"
	break
    }
    foreach c $choices[set choices {}] {
	if {[scorecalc $c $ans] eq $score} {
	    lappend choices $c
	}
    }
    if {![llength $choices]} {
	puts "Bad scoring? nothing fits those scores you gave:"
	foreach a $answers s $scores {
	    puts "  [join $a {}] -> ([lindex $s 0], [lindex $s 1])"
	}
	break
    }
}
