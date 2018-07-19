package require Tcl 8.5
proc alias {name args} {uplevel 1 [list interp alias {} $name {} {*}$args]}

# Engine for elementwise operations between matrices
proc elementwiseMatMat {lambda A B} {
    set C {}
    foreach rA $A rB $B {
	set rC {}
	foreach vA $rA vB $rB {
	    lappend rC [apply $lambda $vA $vB]
	}
	lappend C $rC
    }
    return $C
}
# Lift some basic math ops
alias m+  elementwiseMatMat {{a b} {expr {$a+$b}}}
alias m-  elementwiseMatMat {{a b} {expr {$a-$b}}}
alias m*  elementwiseMatMat {{a b} {expr {$a*$b}}}
alias m/  elementwiseMatMat {{a b} {expr {$a/$b}}}
alias m** elementwiseMatMat {{a b} {expr {$a**$b}}}

# Engine for elementwise operations between a matrix and a scalar
proc elementwiseMatSca {lambda A b} {
    set C {}
    foreach rA $A {
	set rC {}
	foreach vA $rA {
	    lappend rC [apply $lambda $vA $b]
	}
	lappend C $rC
    }
    return $C
}
# Lift some basic math ops
alias .+  elementwiseMatSca {{a b} {expr {$a+$b}}}
alias .-  elementwiseMatSca {{a b} {expr {$a-$b}}}
alias .*  elementwiseMatSca {{a b} {expr {$a*$b}}}
alias ./  elementwiseMatSca {{a b} {expr {$a/$b}}}
alias .** elementwiseMatSca {{a b} {expr {$a**$b}}}
