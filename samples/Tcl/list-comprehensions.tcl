package require Tcl 8.5

# from http://wiki.tcl.tk/12574
proc lcomp {expression args} {
    # Check the number of arguments.
    if {[llength $args] < 2} {
        error "wrong # args: should be \"lcomp expression var1 list1\
            ?... varN listN? ?condition?\""
    }

    # Extract condition from $args, or use default.
    if {[llength $args] % 2 == 1} {
        set condition [lindex $args end]
        set args [lrange $args 0 end-1]
    } else {
        set condition 1
    }

    # Collect all var/list pairs and store in reverse order.
    set varlst [list]
    foreach {var lst} $args {
        set varlst [concat [list $var] [list $lst] $varlst]
    }

    # Actual command to be executed, repeatedly.
    set script {lappend result [subst $expression]}

    # If necessary, make $script conditional.
    if {$condition ne "1"} {
        set script [list if $condition $script]
    }

    # Apply layers of foreach constructs around $script.
    foreach {var lst} $varlst {
        set script [list foreach $var $lst $script]
    }

    # Do it!
    set result [list]
    {*}$script ;# Change to "eval $script" if using Tcl 8.4 or older.
    return $result
}

set range {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20}
puts [lcomp {$x $y $z} x $range y $range z $range {$x < $y && $x**2 + $y**2 == $z**2}]
