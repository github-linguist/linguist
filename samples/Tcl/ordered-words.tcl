package require http

# Pick the ordered words (of maximal length) from a list
proc chooseOrderedWords list {
    set len 0
    foreach word $list {
	# Condition to determine whether a word is ordered; are its characters
	# in sorted order?
	if {$word eq [join [lsort [split $word ""]] ""]} {
	    if {[string length $word] > $len} {
		set len [string length $word]
		set orderedOfMaxLen {}
	    }
	    if {[string length $word] == $len} {
		lappend orderedOfMaxLen $word
	    }
	}
    }
    return $orderedOfMaxLen
}

# Get the dictionary and print the ordered words from it
set t [http::geturl "http://www.puzzlers.org/pub/wordlists/unixdict.txt"]
puts [chooseOrderedWords [http::data $t]]
http::cleanup $t
