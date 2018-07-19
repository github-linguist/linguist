package require Tcl 8.5
package require struct::set

# Core sequence generator engine; stores in $R and $S globals
set R {R:-> 1}
set S {S:-> 2}
proc buildSeq {n} {
    global R S
    set ctr [expr {max([lindex $R end],[lindex $S end])}]
    while {[llength $R] <= $n || [llength $S] <= $n} {
	set idx [expr {min([llength $R],[llength $S]) - 1}]
	if {[incr ctr] == [lindex $R $idx]+[lindex $S $idx]} {
	    lappend R $ctr
	} else {
	    lappend S $ctr
	}
    }
}

# Accessor procedures
proc ffr {n} {
    buildSeq $n
    lindex $::R $n
}
proc ffs {n} {
    buildSeq $n
    lindex $::S $n
}

# Show some things about the sequence
for {set i 1} {$i <= 10} {incr i} {
    puts "R($i) = [ffr $i]"
}
puts "Considering {1..1000} vs {R(i)|i\u2208\[1,40\]}\u222a{S(i)|i\u2208\[1,960\]}"
for {set i 1} {$i <= 1000} {incr i} {lappend numsInSeq $i}
for {set i 1} {$i <= 40} {incr i} {
    lappend numsRS [ffr $i]
}
for {set i 1} {$i <= 960} {incr i} {
    lappend numsRS [ffs $i]
}
puts "set sizes: [struct::set size $numsInSeq] vs [struct::set size $numsRS]"
puts "set equality: [expr {[struct::set equal $numsInSeq $numsRS]?{yes}:{no}}]"
