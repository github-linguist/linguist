package require Tcl 8.5
package require Tk

namespace import tcl::mathop::\[-+\]    ;# Shorter coordinate math
proc yinyang {c x y r {colors {white black}}} {
    lassign $colors a b
    set tt [expr {$r * 2 / 3.0}]
    set h [expr {$r / 2.0}]
    set t [expr {$r / 3.0}]
    set s [expr {$r / 6.0}]
    $c create arc [- $x $r] [- $y $r] [+ $x $r] [+ $y $r] \
	-fill $a -outline {} -extent 180 -start 90
    $c create arc [- $x $r] [- $y $r] [+ $x $r] [+ $y $r] \
	-fill $b -outline {} -extent 180 -start 270
    $c create oval [- $x $h] [- $y $r] [+ $x $h] $y \
	-fill $a -outline {}
    $c create oval [- $x $h] [+ $y $r] [+ $x $h] $y \
	-fill $b -outline {}
    $c create oval [- $x $s] [- $y $tt] [+ $x $s] [- $y $t] \
	-fill $b -outline {}
    $c create oval [- $x $s] [+ $y $tt] [+ $x $s] [+ $y $t] \
	-fill $a -outline {}
}

pack [canvas .c -width 300 -height 300 -background gray50]
yinyang .c 110 110 90
yinyang .c 240 240 40
