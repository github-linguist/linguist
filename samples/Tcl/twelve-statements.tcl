package require Tcl 8.6

# Function to evaluate the truth of a statement
proc tcl::mathfunc::S {idx} {
    upvar 1 state s
    apply [lindex $s [expr {$idx - 1}]] $s
}
# Procedure to count the number of statements which are true
proc S+ args {
    upvar 1 state state
    tcl::mathop::+ {*}[lmap i $args {expr {S($i)}}]
}
# Turn a list of expressions into a list of lambda terms
proc lambdas items {lmap x $items {list state [list expr $x]}}

# Find the truth assignment that produces consistency. And those that are
# near misses too.
proc findTruthMatch {statements} {
    set n [llength $statements]
    for {set i 0} {$i < 2**$n} {incr i} {
	set state [split [format %0.*b $n $i] ""]
	set truths [lmap f $statements {apply $f [lambdas $state]}]
	set counteq [tcl::mathop::+ {*}[lmap s $state t $truths {expr {
	    $s == $t
	}}]]
	if {$counteq == $n} {
	    lappend exact $state
	} elseif {$counteq == $n-1} {
	    set j 0
	    foreach s $state t $truths {
		incr j
		if {$s != $t} {
		    lappend differ $state $j
		    break
		}
	    }
	}
    }
    return [list $exact $differ]
}

# Rendering code
proc renderstate state {
    return ([join [lmap s $state {
	incr i
	expr {$s ? "S($i)" : "\u00acS($i)"}
    }] "\u22c0"])
}

# The statements, encoded as expressions
set statements {
    {[llength $state] == 12}
    {[S+ 7 8 9 10 11 12] == 3}
    {[S+ 2 4 6 8 10 12] == 2}
    {S(5) ? S(6) && S(7) : 1}
    {[S+ 2 3 4] == 0}
    {[S+ 1 3 5 7 9 11] == 4}
    {S(2) != S(3)}
    {S(7) ? S(5) && S(6) : 1}
    {[S+ 1 2 3 4 5 6] == 3}
    {S(11) && S(12)}
    {[S+ 7 8 9] == 1}
    {[S+ 1 2 3 4 5 6 7 8 9 10 11] == 4}
}
# Find the truth assignment(s) that give consistency
lassign [findTruthMatch [lambdas $statements]] exact differ
# Print the results
foreach state $exact {
    puts "exact match\t[renderstate $state ]"
}
foreach {state j} $differ {
    puts "almost found\t[renderstate $state] \u21d2 [expr {[lindex $state $j-1]?"\u00ac":{}}]S($j)"
}
