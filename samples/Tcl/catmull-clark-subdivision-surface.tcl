package require Tcl 8.5

# Use math functions and operators as commands (Lisp-like).
namespace path {tcl::mathfunc tcl::mathop}

# Add 3 points.
proc add3 {A B C} {
    lassign $A Ax Ay Az
    lassign $B Bx By Bz
    lassign $C Cx Cy Cz
    list [+ $Ax $Bx $Cx] [+ $Ay $By $Cy] [+ $Az $Bz $Cz]
}

# Multiply a point by a constant.
proc mulC {m A} {
    lassign $A x y z
    list [* $m $x] [* $m $y] [* $m $z]
}

# Take the centroid of a set of points.
# Note that each of the arguments is a *list* of coordinate triples
# This makes things easier later.
proc centroid args {
    set x [set y [set z 0.0]]
    foreach plist $args {
	incr n [llength $plist]
	foreach p $plist {
	    lassign $p px py pz
	    set x [+ $x $px]
	    set y [+ $y $py]
	    set z [+ $z $pz]
	}
    }
    set n [double $n]
    list [/ $x $n] [/ $y $n] [/ $z $n]
}

# Select from the list the value from each of the indices in the *lists*
# in the trailing arguments.
proc selectFrom {list args} {
    foreach is $args {foreach i $is {lappend r [lindex $list $i]}}
    return $r
}

# Rotate a list.
proc lrot {list {n 1}} {
    set n [% $n [llength $list]]
    list {*}[lrange $list $n end] {*}[lrange $list 0 [incr n -1]]
}

# Generate an edge by putting the smaller coordinate index first.
proc edge {a b} {
    list [min $a $b] [max $a $b]
}

# Perform one step of Catmull-Clark subdivision of a surface.
proc CatmullClark {points faces} {
    # Generate the new face-points and list of edges, plus some lookup tables.
    set edges {}
    foreach f $faces {
	set ps [selectFrom $points $f]
	set fp [centroid $ps]
	lappend facepoints $fp
	foreach p $ps {
	    lappend fp4p($p) $fp
	}
	foreach p1 $f p2 [lrot $f] {
	    set e [edge $p1 $p2]
	    if {$e ni $edges} {
		lappend edges $e
	    }
	    lappend fp4e($e) $fp
	}
    }

    # Generate the new edge-points and mid-points of edges, and a few more
    # lookup tables.
    set i [+ [llength $points] [llength $faces]]
    foreach e $edges {
	set ep [selectFrom $points $e]
	if {[llength $fp4e($e)] > 1} {
	    set mid [centroid $ep $fp4e($e)]
	} else {
	    set mid [centroid $ep]
	    foreach p $ep {
		lappend ep_heavy($p) $mid
	    }
	}
	lappend edgepoints $mid
	set en4e($e) $i
	foreach p $ep {
	    lappend ep4p($p) $mid
	}
	incr i
    }

    # Generate the new vertex points with our lookup tables.
    foreach p $points {
	if {[llength $fp4p($p)] >= 4} {
	    set n [llength $fp4p($p)]
	    lappend newPoints [add3 [mulC [/ [- $n 3.0] $n] $p] \
		    [mulC [/ 1.0 $n] [centroid $fp4p($p)]] \
		    [mulC [/ 2.0 $n] [centroid $ep4p($p)]]]
	} else {
	    # Update a point on the edge of a hole. This formula is not
	    # described on the WP page, but produces a nice result.
	    lappend newPoints [centroid $ep_heavy($p) [list $p $p]]
	}
    }

    # Now compute the new set of quadrilateral faces.
    set i [llength $points]
    foreach f $faces {
	foreach a $f b [lrot $f] c [lrot $f -1] {
	    lappend newFaces [list \
		    $a $en4e([edge $a $b]) $i $en4e([edge $c $a])]
	}
	incr i
    }

    list [concat $newPoints $facepoints $edgepoints] $newFaces
}
