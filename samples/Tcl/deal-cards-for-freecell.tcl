proc rnd {{*r seed}} {
    upvar 1 ${*r} r
    expr {[set r [expr {($r * 214013 + 2531011) & 0x7fffffff}]] >> 16}
}
proc show cards {
    set suits {\u2663 \u2666 \u2665 \u2660}
    set values {A 2 3 4 5 6 7 8 9 T J Q K}
    for {set i 0} {$i < 52} {incr i} {
	set c [lindex $cards $i]
	puts -nonewline [format "  \033\[%dm%s\033\[m%s" [expr {32-(1+$c)%4/2}] \
	    [lindex $suits [expr {$c % 4}]] [lindex $values [expr {$c / 4}]]]
	if {($i&7)==7 || $i==51} {puts ""}
    }
}
proc deal {seed} {
    for {set i 0} {$i < 52} {incr i} {lappend cards [expr {51 - $i}]}
    for {set i 0} {$i < 51} {incr i} {
	set j [expr {51 - [rnd]%(52-$i)}]
	set tmp [lindex $cards $i]
	lset cards $i [lindex $cards $j]
	lset cards $j $tmp
    }
    return $cards
}

if {![scan =[lindex $argv 0]= =%d= s] || $s <= 0} {
    set s 11982
}
set cards [deal $s]
puts "Hand $s"
show $cards
