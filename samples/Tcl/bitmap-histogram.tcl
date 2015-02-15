package require Tcl 8.5
package require Tk

proc convert_to_blackandwhite {filename} {
    set img [image create photo]
    readPPM $img $filename
    grayscale $img
    set hist [histogram $img]
    set median [median $img $hist]
    blackandwhite $img $median
    output_ppm $img bw_$filename
}

proc histogram {image} {
    set hist [dict create]
    for {set x 0} {$x < [image width $image]} {incr x} {
        for {set y 0} {$y < [image height $image]} {incr y} {
            dict incr hist [luminance {*}[$image get $x $y]]
        }
    }
    return $hist
}

proc luminance {r g b} {
    expr {
        int(0.2126*$r + 0.7152*$g + 0.0722*$b)
    }
}

proc median {img hist} {
    set sum [expr {[image width $img] * [image height $img]}]
    set total 0
    foreach luminance [lsort -integer [dict keys $hist]] {
        incr total [dict get $hist $luminance]
        if {$total > $sum / 2} break
    }
    return $luminance
}

proc blackandwhite {image median} {
    for {set x 0} {$x < [image width $image]} {incr x} {
        for {set y 0} {$y < [image height $image]} {incr y} {
            if {[luminance {*}[$image get $x $y]] < $median} {
                $image put black -to $x $y
            } else {
                $image put white -to $x $y
            }
        }
    }
}
