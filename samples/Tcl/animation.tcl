package require Tk
set s "Hello World! "
set dir 0
# Periodic animation callback
proc animate {} {
    global dir s
    if {$dir} {
        set s [string range $s 1 end][string index $s 0]
    } else {
        set s [string index $s end][string range $s 0 end-1]
    }
    # We will run this code ~8 times a second (== 125ms delay)
    after 125 animate
}
# Make the label (constant width font looks better)
pack [label .l -textvariable s -font {Courier 14}]
# Make a mouse click reverse the direction
bind .l <Button-1> {set dir [expr {!$dir}]}
# Start the animation
animate
