package require Tcl 8.5
package require Tk

# Farm out grabbing the screen to an external program.
# If it was just for a Tk window, we'd use the tkimg library instead
proc grabScreen {image} {
    set pipe [open {|xwd -root -silent | convert xwd:- ppm:-} rb]
    $image put [read $pipe]
    close $pipe
}
# Get the RGB data for a particular pixel (global coords)
proc getPixelAtPoint {x y} {
    set buffer [image create photo]
    grabScreen $buffer
    set data [$image get $x $y]
    image delete $buffer
    return $data
}

# Demo...
puts [format "pixel at mouse: (%d,%d,%d)" \
    {*}[getPixelAtPoint {*}[winfo pointerxy .]]]
