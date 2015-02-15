package require Tk

# Set the color of a pixel
proc applyMedian {srcImage x y -> dstImage} {
    set x0 [expr {$x==0 ? 0 : $x-1}]
    set y0 [expr {$y==0 ? 0 : $y-1}]
    set x1 $x
    set y1 $y
    set x2 [expr {$x+1==[image width $srcImage]  ? $x : $x+1}]
    set y2 [expr {$y+1==[image height $srcImage] ? $y : $y+1}]

    set r [set g [set b {}]]
    foreach X [list $x0 $x1 $x2] {
	foreach Y [list $y0 $y1 $y2] {
	    lassign [$srcImage get $X $Y] rPix gPix bPix
	    lappend r $rPix
	    lappend g $gPix
	    lappend b $bPix
	}
    }
    set r [lindex [lsort -integer $r] 4]
    set g [lindex [lsort -integer $g] 4]
    set b [lindex [lsort -integer $b] 4]
    $dstImage put [format "#%02x%02x%02x" $r $g $b] -to $x $y
}
# Apply the filter to the whole image
proc medianFilter {srcImage {dstImage ""}} {
    if {$dstImage eq ""} {
	set dstImage [image create photo]
    }
    set w [image width $srcImage]
    set h [image height $srcImage]
    for {set x 0} {$x < $w} {incr x} {
	for {set y 0} {$y < $h} {incr y} {
	    applyMedian $srcImage $x $y -> $dstImage
	}
    }
    return $dstImage
}

# Demonstration code using the Tk widget demo's teapot image
image create photo teapot -file $tk_library/demos/images/teapot.ppm
pack [labelframe .src -text Source] -side left
pack [label .src.l -image teapot]
update
pack [labelframe .dst -text Median] -side left
pack [label .dst.l -image [medianFilter teapot]]
