package require Tk

# The actual plotting engine
proc plotxy {canvas xs ys} {
    global xfac yfac
    set maxx [tcl::mathfunc::max {*}$xs]
    set maxy [tcl::mathfunc::max {*}$ys]
    set xfac [expr {[winfo width $canvas] * 0.8/$maxx}]
    set yfac [expr {[winfo height $canvas] * 0.8/$maxy}]
    scale $canvas x 0 $maxx $xfac
    scale $canvas y 0 $maxy $yfac
    foreach x $xs y $ys {
        dot $canvas [expr {$x*$xfac}] [expr {$y*$yfac}] -fill red
    }
}
# Rescales the contents of the given canvas
proc scale {canvas direction from to fac} {
    set f [expr {$from*$fac}]
    set t [expr {$to*$fac}]
    switch -- $direction {
        x {
            set f [expr {$from * $fac}]
            set t [expr {$to * $fac}]
            $canvas create line $f 0 $t 0
            $canvas create text $f 0 -anchor nw -text $from
            $canvas create text $t 0 -anchor n -text $to

        }
        y {
            set f [expr {$from * -$fac}]
            set t [expr {$to * -$fac}]
            $canvas create line 0 $f 0 $t
            $canvas create text 0 $f -anchor se -text $from
            $canvas create text 0 $t -anchor e -text $to
        }
    }
}
# Helper to make points, which are otherwise not a native item type
proc dot {canvas x y args} {
    set id [$canvas create oval [expr {$x-3}] [expr {-$y-3}] \
                [expr {$x+3}] [expr {-$y+3}]]
    $canvas itemconfigure $id {*}$args
}

pack [canvas .c -background white]
update
set xs {0   1    2    3    4    5     6     7     8     9}
set ys {2.7 2.8 31.4 38.1 58.0 76.2 100.5 130.0 149.3 180.0}
plotxy .c $xs $ys
.c config -scrollregion [.c bbox all]
.c move all 20 20

# Save image (this is the only part that requires an external library)
package require Img
set im [image create photo -data .c]
$im write plotxy.png -format PNG
