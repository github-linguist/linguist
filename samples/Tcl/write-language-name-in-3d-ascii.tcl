package require Tcl 8.5

proc mergeLine {upper lower} {
    foreach u [split $upper ""] l [split $lower ""] {
	lappend result [expr {$l in {" " ""} ? $u : $l}]
    }
    return [join $result ""]
}
proc printLines lines {
    set n [llength $lines]
    foreach line $lines {
	set indent [string repeat " " $n]
	lappend upper $indent[string map {"/ " "/\\"} [
		string map {" " "   " "*" "///"} "$line "]]
	lappend lower $indent[string map {"\\ " "\\/"} [
		string map {" " "   " "*" "\\\\\\"} "$line "]]
	incr n -1
    }
    # Now do some line merging to strengthen the visual effect
    set p [string repeat " " [string length [lindex $upper 0]]]
    foreach u $upper l $lower {
	puts [mergeLine $p $u]
	set p $l
    }
    puts $p
}

set lines {
    {*****   *}
    {  *     *}
    {  * *** *}
    {  * *   *}
    {  * *   *}
    {  * *** *}
}
printLines $lines
