package require Tcl 8.5
package require http

# Fetch the words
set t [http::geturl http://www.puzzlers.org/pub/wordlists/unixdict.txt]
set wordlist [split [http::data $t] \n]
http::cleanup $t

# Build hash table for speed
foreach word $wordlist {
    set reversed([string reverse $word]) "dummy"
}

# Find where a reversal exists
foreach word $wordlist {
    if {[info exists reversed($word)] && $word ne [string reverse $word]} {
	# Remove to prevent pairs from being printed twice
	unset reversed([string reverse $word])
	# Add to collection of pairs
	set pairs($word/[string reverse $word]) "dummy"
    }
}
set pairlist [array names pairs] ;# NB: pairs are in *arbitrary* order

# Report what we've found
puts "Found [llength $pairlist] reversed pairs"
foreach pair $pairlist {
    puts "Example: $pair"
    if {[incr i]>=5} break
}
