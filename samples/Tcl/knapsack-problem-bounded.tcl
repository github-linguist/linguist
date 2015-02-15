# The list of items to consider, as list of lists
set items {
    {map			9	150	1}
    {compass			13	35	1}
    {water			153	200	2}
    {sandwich			50	60	2}
    {glucose			15	60	2}
    {tin			68	45	3}
    {banana			27	60	3}
    {apple			39	40	3}
    {cheese			23	30	1}
    {beer			52	10	3}
    {{suntan cream}		11	70	1}
    {camera			32	30	1}
    {t-shirt			24	15	2}
    {trousers			48	10	2}
    {umbrella			73	40	1}
    {{waterproof trousers}	42	70	1}
    {{waterproof overclothes}	43	75	1}
    {note-case			22	80	1}
    {sunglasses			7	20	1}
    {towel			18	12	2}
    {socks			4	50	1}
    {book			30	10	2}
}

# Simple extraction functions
proc countednames {chosen} {
    set names {}
    foreach item $chosen {
	lappend names [lindex $item 3] [lindex $item 0]
    }
    return $names
}
proc weight {chosen} {
    set weight 0
    foreach item $chosen {
	incr weight [expr {[lindex $item 3] * [lindex $item 1]}]
    }
    return $weight
}
proc value {chosen} {
    set value 0
    foreach item $chosen {
	incr value [expr {[lindex $item 3] * [lindex $item 2]}]
    }
    return $value
}

# Recursive function for searching over all possible choices of items
proc knapsackSearch {items {chosen {}}} {
    # If we've gone over the weight limit, stop now
    if {[weight $chosen] > 400} {
	return
    }
    # If we've considered all of the items (i.e., leaf in search tree)
    # then see if we've got a new best choice.
    if {[llength $items] == 0} {
	global best max
	set v [value $chosen]
	if {$v > $max} {
	    set max $v
	    set best $chosen
	}
	return
    }
    # Branch, so recurse for chosing the current item or not
    set this [lindex $items 0]
    set rest [lrange $items 1 end]
    knapsackSearch $rest $chosen
    for {set i 1} {$i<=[lindex $this 3]} {incr i} {
	knapsackSearch $rest \
	    [concat $chosen [list [lreplace $this end end $i]]]
    }
}

# Initialize a few global variables
set best {}
set max 0
# Do the brute-force search
knapsackSearch $items
# Pretty-print the results
puts "Best filling has weight of [expr {[weight $best]/100.0}]kg and score [value $best]"
puts "Best items:"
foreach {count item} [countednames $best] {
    puts "\t$count * $item"
}
