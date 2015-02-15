package require Tcl 8.5

# Compute tan(atan(p)+atan(q)) using rationals
proc tadd {p q} {
    lassign $p pp pq
    lassign $q qp qq
    set topp [expr {$pp*$qq + $qp*$pq}]
    set topq [expr {$pq*$qq}]
    set prodp [expr {$pp*$qp}]
    set prodq [expr {$pq*$qq}]
    set lowp [expr {$prodq - $prodp}]
    set resultp [set gcd1 [expr {$topp * $prodq}]]
    set resultq [set gcd2 [expr {$topq * $lowp}]]
    # Critical! Normalize using the GCD
    while {$gcd2 != 0} {
	lassign [list $gcd2 [expr {$gcd1 % $gcd2}]] gcd1 gcd2
    }
    list [expr {$resultp / abs($gcd1)}] [expr {$resultq / abs($gcd1)}]
}
proc termTan {n a b} {
    if {$n < 0} {
	set n [expr {-$n}]
	set a [expr {-$a}]
    }
    if {$n == 1} {
	return [list $a $b]
    }
    set k [expr {$n - [set m [expr {$n / 2}]]*2}]
    set t2 [termTan $m $a $b]
    set m2 [tadd $t2 $t2]
    if {$k == 0} {
	return $m2
    }
    return [tadd [termTan $k $a $b] $m2]
}
proc machinTan {terms} {
    set sum {0 1}
    foreach term $terms {
	set sum [tadd $sum [termTan {*}$term]]
    }
    return $sum
}

# Assumes that the formula is in the very specific form below!
proc parseFormula {formula} {
    set RE {(-?\s*\d*\s*\*?)\s*arctan\s*\(\s*(-?\s*\d+)\s*/\s*(-?\s*\d+)\s*\)}
    set nospace {" " "" "*" ""}
    foreach {all n a b} [regexp -inline -all $RE $formula] {
	if {![regexp {\d} $n]} {append n 1}
	lappend result [list [string map $nospace $n] [string map $nospace $a] [string map $nospace $b]]
    }
    return $result
}

foreach formula {
    "pi/4 = arctan(1/2) + arctan(1/3)"
    "pi/4 = 2*arctan(1/3) + arctan(1/7)"
    "pi/4 = 4*arctan(1/5) - arctan(1/239)"
    "pi/4 = 5*arctan(1/7) + 2*arctan(3/79)"
    "pi/4 = 5*arctan(29/278) + 7*arctan(3/79)"
    "pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)"
    "pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)"
    "pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)"
    "pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)"
    "pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)"
    "pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)"
    "pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)"
    "pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)"
    "pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)"
    "pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)"
    "pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)"
    "pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)"
} {
    if {[tcl::mathop::== {*}[machinTan [parseFormula $formula]]]} {
	puts "Yes! '$formula' is true"
    } else {
	puts "No! '$formula' not true"
    }
}
