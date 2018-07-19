package require http

variable PLAUSIBILITY_RATIO 2.0
proc plausible {description x y} {
    variable PLAUSIBILITY_RATIO
    puts "  Checking plausibility of: $description"
    if {$x > $PLAUSIBILITY_RATIO * $y} {
	set conclusion "PLAUSIBLE"
	set fmt "As we have counts of %i vs %i words, a ratio of %.1f times"
	set result true
    } elseif {$x > $y} {
	set conclusion "IMPLAUSIBLE"
	set fmt "As although we have counts of %i vs %i words,"
	append fmt " a ratio of %.1f times does not make it plausible"
	set result false
    } else {
	set conclusion "IMPLAUSIBLE, probably contra-indicated"
	set fmt "As we have counts of %i vs %i words, a ratio of %.1f times"
	set result false
    }
    puts [format "    %s.\n    $fmt" $conclusion $x $y [expr {double($x)/$y}]]
    return $result
}

set t [http::geturl http://www.puzzlers.org/pub/wordlists/unixdict.txt]
set words [split [http::data $t] "\n"]
http::cleanup $t
foreach {name pattern} {ie (?:^|[^c])ie ei (?:^|[^c])ei cie cie cei cei} {
    set count($name) [llength [lsearch -nocase -all -regexp $words $pattern]]
}

puts "Checking plausibility of \"I before E except after C\":"
if {
    [plausible "I before E when not preceded by C" $count(ie) $count(ei)] &&
    [plausible "E before I when preceded by C" $count(cei) $count(cie)]
} then {
    puts "\nOVERALL IT IS PLAUSIBLE!"
} else {
    puts "\nOVERALL IT IS IMPLAUSIBLE!"
}
puts "\n(To be plausible, one word count must exceed another by\
	$PLAUSIBILITY_RATIO times)"
