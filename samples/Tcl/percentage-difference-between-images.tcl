package require Tk

proc imageDifference {img1 img2} {
    if {
	[image width $img1] != [image width $img2] ||
	[image height $img1] != [image height $img2]
    } then {
	return -code error "images are different size"
    }
    set diff 0
    for {set x 0} {$x<[image width $img1]} {incr x} {
	for {set y 0} {$y<[image height $img1]} {incr y} {
	    lassign [$img1 get $x $y] r1 g1 b1
	    lassign [$img2 get $x $y] r2 g2 b2
	    incr diff [expr {abs($r1-$r2)+abs($g1-$g2)+abs($b1-$b2)}]
	}
    }
    expr {$diff/double($x*$y*3*255)}
}

# Package only used for JPEG loader
package require Img
image create photo lenna50 -file lenna50.jpg
image create photo lenna100 -file lenna100.jpg
puts "difference is [expr {[imageDifference lenna50 lenna100]*100.}]%"
exit ;# Need explicit exit here; don't want a GUI
