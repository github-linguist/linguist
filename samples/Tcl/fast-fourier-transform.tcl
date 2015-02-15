package require math::constants
package require math::fourier

math::constants::constants pi
# Helper functions
proc wave {samples cycles} {
    global pi
    set wave {}
    set factor [expr {2*$pi * $cycles / $samples}]
    for {set i 0} {$i < $samples} {incr i} {
	lappend wave [expr {sin($factor * $i)}]
    }
    return $wave
}
proc printwave {waveName {format "%7.3f"}} {
    upvar 1 $waveName wave
    set out [format "%-6s" ${waveName}:]
    foreach value $wave {
	append out [format $format $value]
    }
    puts $out
}
proc waveMagnitude {wave} {
    set out {}
    foreach value $wave {
	lassign $value re im
	lappend out [expr {hypot($re, $im)}]
    }
    return $out
}

set wave [wave 16 3]
printwave wave
# Uses FFT if input length is power of 2, and a less efficient algorithm otherwise
set fft [math::fourier::dft $wave]
# Convert to magnitudes for printing
set fft2 [waveMagnitude $fft]
printwave fft2
