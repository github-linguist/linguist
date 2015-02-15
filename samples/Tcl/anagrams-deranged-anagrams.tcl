package require Tcl 8.5
package require http

# Fetch the words
set t [http::geturl "http://www.puzzlers.org/pub/wordlists/unixdict.txt"]
set wordlist [split [http::data $t] \n]
http::cleanup $t

# Group by characters in word
foreach word $wordlist {
    dict lappend w [lsort [split $word ""]] [split $word ""]
}

# Deranged test
proc deranged? {l1 l2} {
    foreach c1 $l1 c2 $l2 {
	if {$c1 eq $c2} {return 0}
    }
    return 1
}

# Get a deranged pair from an anagram set, if one exists
proc getDeranged {words} {
    foreach l1 [lrange $words 0 end-1] {
	foreach l2 [lrange $words 1 end] {
	    if {[deranged? $l1 $l2]} {
		return [list $l1 $l2 1]
	    }
	}
    }
    return {{} {} 0}
}

# Find the max-length deranged anagram
set count 0
set candidates {}
set max 0
dict for {k words} $w {
    incr count [expr {[llength $words] > 1}]
    if {[llength $k] > $max && [lassign [getDeranged $words] l1 l2]} {
	set max [llength $l1]
	lappend candidates [join $l1 ""],[join $l2 ""]
    }
}

# Print out what we found
puts "[llength $wordlist] words"
puts "[dict size $w] potential anagram-groups"
puts "$count real anagram-groups"
foreach pair $candidates {
    puts "considered candidate pairing: $pair"
}
puts "MAXIMAL DERANGED ANAGRAM: LENGTH $max\n\t[lindex $candidates end]"
