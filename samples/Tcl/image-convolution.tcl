package require Tk

# Function for clamping values to those that we can use with colors
proc tcl::mathfunc::luma channel {
    set channel [expr {round($channel)}]
    if {$channel < 0} {
	return 0
    } elseif {$channel > 255} {
	return 255
    } else {
	return $channel
    }
}
# Applies a convolution kernel to produce a single pixel in the destination
proc applyKernel {srcImage x y -- kernel -> dstImage} {
    set x0 [expr {$x==0 ? 0 : $x-1}]
    set y0 [expr {$y==0 ? 0 : $y-1}]
    set x1 $x
    set y1 $y
    set x2 [expr {$x+1==[image width $srcImage]  ? $x : $x+1}]
    set y2 [expr {$y+1==[image height $srcImage] ? $y : $y+1}]

    set r [set g [set b 0.0]]
    foreach X [list $x0 $x1 $x2] kcol $kernel {
	foreach Y [list $y0 $y1 $y2] k $kcol {
	    lassign [$srcImage get $X $Y] rPix gPix bPix
	    set r [expr {$r + $k * $rPix}]
	    set g [expr {$g + $k * $gPix}]
	    set b [expr {$b + $k * $bPix}]
	}
    }

    $dstImage put [format "#%02x%02x%02x" \
		       [expr {luma($r)}] [expr {luma($g)}] [expr {luma($b)}]]\
	-to $x $y
}
# Apply a convolution kernel to a whole image
proc convolve {srcImage kernel {dstImage ""}} {
    if {$dstImage eq ""} {
	set dstImage [image create photo]
    }
    set w [image width $srcImage]
    set h [image height $srcImage]
    for {set x 0} {$x < $w} {incr x} {
	for {set y 0} {$y < $h} {incr y} {
	    applyKernel $srcImage $x $y -- $kernel -> $dstImage
	}
    }
    return $dstImage
}

# Demonstration code using the teapot image from Tk's widget demo
image create photo teapot -file $tk_library/demos/images/teapot.ppm
pack [labelframe .src -text Source] -side left
pack [label .src.l -image teapot]
foreach {label kernel} {
    Emboss {
	{-2. -1. 0.}
	{-1.  1. 1.}
	{ 0.  1. 2.}
    }
    Sharpen {
	{-1. -1. -1}
	{-1.  9. -1}
	{-1. -1. -1}
    }
    Blur {
	{.1111 .1111 .1111}
	{.1111 .1111 .1111}
	{.1111 .1111 .1111}
    }
} {
    set name [string tolower $label]
    update
    pack [labelframe .$name -text $label] -side left
    pack [label .$name.l -image [convolve teapot $kernel]]
}
