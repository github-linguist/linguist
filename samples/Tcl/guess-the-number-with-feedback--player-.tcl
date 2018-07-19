set from 1
set to 10
fconfigure stdout -buffering none
while 1 {
    set guess [expr {($from+$to) / 2}]
    puts -nonewline "Guess: $guess\tWas it lower (<) equal (=) or higher (>)? "
    switch -- [gets stdin] {
	< { set from [expr {$guess + 1}] }
	> { set to [expr {$guess - 1}] }
	= {
	    puts "Found it: $guess"
	    break
	}
	default {
	    puts "What sort of a response was that?!"
	}
    }
    if {$to < $from} {
	puts "No answer possible?!"
	break
    }
}
