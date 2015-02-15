package require Tk

proc generate {img width height} {
    set data {}
    for {set i 0} {$i<$height} {incr i} {
	set line {}
	for {set j 0} {$j<$width} {incr j} {
	    lappend line [lindex "#000000 #FFFFFF" [expr {rand() < 0.5}]]
	}
	lappend data $line
    }
    $img put $data
}

set time 0.0
set count 0

proc looper {} {
    global time count
    set t [lindex [time {generate noise 320 240}] 0]
    set time [expr {$time + $t}]
    if {[incr count] >= 30} {
	set time [expr {$time / 1000000.0}]
	set fps [expr {$count / $time}]
	puts [format "%d frames in %3.2f seconds (%f FPS)" $count $time $fps]
	set time 0.0
	set count 0
    }
    after 1 looper
}

image create photo noise -width 320 -height 240
pack [label .l -image noise]
update
looper
