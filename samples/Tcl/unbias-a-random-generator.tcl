# 1,0 random generator factory with 1 appearing 1/N'th of the time
proc randN n {expr {rand()*$n < 1}}

# uses a biased generator of 1 or 0, to create an unbiased one
proc unbiased {biased} {
    while 1 {
	if {[set a [eval $biased]] != [eval $biased]} {return $a}
    }
}

for {set n 3} {$n <= 6} {incr n} {
    set biased [list randN $n]
    for {set i 0;array set c {0 0 1 0}} {$i < 1000000} {incr i} {
	incr c([eval $biased])
    }
    puts [format "  biased %d => #0=%d #1=%d ratio=%.2f%%" $n $c(0) $c(1) \
	      [expr {100.*$c(1)/$i}]]
    for {set i 0;array set c {0 0 1 0}} {$i < 1000000} {incr i} {
	incr c([unbiased $biased])
    }
    puts [format "unbiased %d => #0=%d #1=%d ratio=%.2f%%" $n $c(0) $c(1) \
	      [expr {100.*$c(1)/$i}]]
}
