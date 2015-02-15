proc meanTime {times} {
    set secsPerRad [expr {60 * 60 * 12 / atan2(0,-1)}]
    set sumSin [set sumCos 0.0]
    foreach t $times {
	# Convert time to count of seconds from midnight
	scan $t "%02d:%02d:%02d" h m s
	incr s [expr {[incr m [expr {$h * 60}]] * 60}]
	# Feed into averaging
	set sumSin [expr {$sumSin + sin($s / $secsPerRad)}]
	set sumCos [expr {$sumCos + cos($s / $secsPerRad)}]
    }
    # Don't need to divide by counts; atan2() cancels that out
    set a [expr {round(atan2($sumSin, $sumCos) * $secsPerRad)}]
    # Convert back to human-readable
    format "%02d:%02d:%02d" [expr {$a / 60 / 60 % 24}] [expr {$a / 60 % 60}] [expr {$a % 60}]
}

puts [meanTime {23:00:17 23:40:20 00:12:45 00:17:19}]
