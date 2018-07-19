package require Tk

proc grayscale image {
    set w [image width $image]
    set h [image height $image]
    for {set x 0} {$x<$w} {incr x} {
        for {set y 0} {$y<$h} {incr y} {
            lassign [$image get $x $y] r g b
            set l [expr {int(0.2126*$r + 0.7152*$g + 0.0722*$b)}]
            $image put [format "#%02x%02x%02x" $l $l $l] -to $x $y
        }
    }
}
