proc hailstone n {
    while 1 {
	lappend seq $n
	if {$n == 1} {return $seq}
	set n [expr {$n & 1 ? $n*3+1 : $n/2}]
    }
}

set h27 [hailstone 27]
puts "h27 len=[llength $h27]"
puts "head4 = [lrange $h27 0 3]"
puts "tail4 = [lrange $h27 end-3 end]"

set maxlen [set max 0]
for {set i 1} {$i<100000} {incr i} {
    set l [llength [hailstone $i]]
    if {$l>$maxlen} {set maxlen $l;set max $i}
}
puts "max is $max, with length $maxlen"
