package require Tcl 8.5
package require Tk
namespace path ::tcl::mathfunc ;# for [max] function

proc newImage {width height} {
    return [image create photo -width $width -height $height]
}
proc fill {image colour} {
    $image put $colour -to 0 0 [$image cget -width] [$image cget -height]
}
proc setPixel {image colour point} {
    lassign $point x y
    $image put $colour -to [max 0 $x] [max 0 $y]
}
proc getPixel {image point} {
    lassign $point x y
    # [$img get] returns a list: {r g b}; this proc should return a colour value
    format {#%02x%02x%02x} {*}[$image get $x $y]
}

# create the image and display it
set img [newImage 150 150]
label .l -image $img
pack .l

fill $img red

setPixel $img green {40 40}

set rbg [getPixel $img {40 40}]
