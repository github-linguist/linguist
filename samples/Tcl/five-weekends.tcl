package require Tcl 8.5

set months {}
set years {}
for {set year 1900} {$year <= 2100} {incr year} {
    set count [llength $months]
    foreach month {Jan Mar May Jul Aug Oct Dec} {
	set date [clock scan "$month/01/$year" -format "%b/%d/%Y" -locale en_US]
	if {[clock format $date -format %u] == 5} {
	    # Month with 31 days that starts on a Friday => has 5 weekends
	    lappend months "$month $year"
	}
    }
    if {$count == [llength $months]} {
	# No change to number of months; year must've been without
	lappend years $year
    }
}
puts "There are [llength $months] months with five weekends"
puts [join [list {*}[lrange $months 0 4] ... {*}[lrange $months end-4 end]] \n]
puts "There are [llength $years] years without any five-weekend months"
puts [join $years ","]
