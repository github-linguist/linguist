package require struct::list
# Encoding the various expression trees that are possible
set patterns {
    {((A x B) y C) z D}
    {(A x (B y C)) z D}
    {(A x B) y (C z D)}
    {A x ((B y C) z D)}
    {A x (B y (C z D))}
}
# Encoding the various permutations of digits
set permutations [struct::list map [struct::list permutations {a b c d}] \
        {apply {v {lassign $v a b c d; list A $a B $b C $c D $d}}}]
# The permitted operations
set operations {+ - * /}

# Given a list of four integers (precondition not checked!) return a list of
# solutions to the 24 game using those four integers.
proc find24GameSolutions {values} {
    global operations patterns permutations
    set found {}
    # For each possible structure with numbers at the leaves...
    foreach pattern $patterns {
	foreach permutation $permutations {
	    set p [string map [subst {
		a [lindex $values 0].0
		b [lindex $values 1].0
		c [lindex $values 2].0
		d [lindex $values 3].0
	    }] [string map $permutation $pattern]]

            # For each possible structure with operators at the branches...
	    foreach x $operations {
		foreach y $operations {
		    foreach z $operations {
			set e [string map [subst {x $x y $y z $z}] $p]

			# Try to evaluate (div-zero is an issue!) and add it to
			# the result if it is 24
			catch {
			    if {[expr $e] == 24.0} {
				lappend found [string map {.0 {}} $e]
			    }
			}
		    }
		}
	    }
	}
    }
    return $found
}

# Wrap the solution finder into a player
proc print24GameSolutionFor {values} {
    set found [lsort -unique [find24GameSolutions $values]]
    if {![llength $found]} {
	puts "No solution possible"
    } else {
	puts "Total [llength $found] solutions (may include logical duplicates)"
        puts "First solution: [lindex $found 0]"
    }
}
print24GameSolutionFor $argv
