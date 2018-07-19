# Get how the terminal wants to do things...
set videoSeq(reverse) [exec tput rev]
set videoSeq(normal) [exec tput rmso]
proc reverseVideo str {
    global videoSeq
    return "$videoSeq(reverse)${str}$videoSeq(normal)"
}

# The things to print
set inReverse "foo"
set inNormal "bar"

# Print those words
puts "[reverseVideo $inReverse] $inNormal"
