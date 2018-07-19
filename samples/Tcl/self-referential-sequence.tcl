proc nextterm n {
    foreach c [split $n ""] {incr t($c)}
    foreach c {9 8 7 6 5 4 3 2 1 0} {
	if {[info exist t($c)]} {append r $t($c) $c}
    }
    return $r
}
# Local context of lambda term is just for speed
apply {limit {
    #  Build a digit cache; this adds quite a bit of speed
    set done [lrepeat [set l2 [expr {$limit * 100}]] 0]
    # Iterate over search space
    set maxlen 0
    set maxes {}
    for {set i 0} {$i < $limit} {incr i} {
	if {[lindex $done $i]} continue
	# Compute the sequence length for this value (with help from cache)
	set seq {}
	for {set seed $i} {$seed ni $seq} {set seed [nextterm $seed]} {
	    if {$seed < $l2 && [lindex $done $seed]} {
		set len [expr {[llength $seq] + [lindex $done $seed]}]
		break
	    }
	    set len [llength [lappend seq $seed]]
	}
	# What are we going to do about it?
	if {$len > $maxlen} {
	    set maxlen $len
	    set maxes [list $i]
	} elseif {$len == $maxlen} {
	    lappend maxes $i
	}
	# Update the cache with what we have learned
	foreach n $seq {
	    if {$n < $l2} {lset done $n $len}
	    incr len -1
	}
    }
    # Output code
    puts "max length: $maxlen"
    foreach c $maxes {puts $c}
    puts "Sample max-len sequence:"
    set seq {}
    # Rerun the sequence generator for printing; faster for large limits
    for {set seed [lindex $c 0]} {$seed ni $seq} {set seed [nextterm $seed]} {
	lappend seq $seed
        puts "\t$seed"
    }
}} 1000000
