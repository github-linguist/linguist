package require Tcl 8.5
package require Tk

# Make the graphical entities
pack [canvas .c -width 320 -height 200] -fill both -expand 1
.c create line 0 25 320 25 -width 2 -fill grey50 -tags plate
.c create line 1 1 1 1 -tags rod -width 3 -fill black
.c create oval 1 1 2 2 -tags bob -fill yellow -outline black
.c create oval 155 20 165 30 -fill grey50 -outline {} -tags pivot

# Set some vars
set points {}
set Theta   45.0
set dTheta   0.0
set pi       3.1415926535897933
set length 150
set homeX  160

# How to respond to a changing in size of the window
proc resized {width} {
    global homeX
    .c coords plate 0 25 $width 25
    set homeX [expr {$width / 2}]
    .c coords pivot [expr {$homeX-5}] 20 [expr {$homeX+5}] 30
    showPendulum
}

# How to actually arrange the pendulum, mapping the model to the display
proc showPendulum {} {
    global Theta dTheta pi length homeX
    set angle [expr {$Theta * $pi/180}]
    set x [expr {$homeX + $length*sin($angle)}]
    set y [expr {25 + $length*cos($angle)}]
    .c coords rod $homeX 25 $x $y
    .c coords bob [expr {$x-15}] [expr {$y-15}] [expr {$x+15}] [expr {$y+15}]
}

# The dynamic part of the display
proc recomputeAngle {} {
    global Theta dTheta pi length
    set scaling [expr {3000.0/$length**2}]

    # first estimate
    set firstDDTheta [expr {-sin($Theta * $pi/180)*$scaling}]
    set midDTheta [expr {$dTheta + $firstDDTheta}]
    set midTheta [expr {$Theta + ($dTheta + $midDTheta)/2}]
    # second estimate
    set midDDTheta [expr {-sin($midTheta * $pi/180)*$scaling}]
    set midDTheta [expr {$dTheta + ($firstDDTheta + $midDDTheta)/2}]
    set midTheta [expr {$Theta + ($dTheta + $midDTheta)/2}]
    # Now we do a double-estimate approach for getting the final value
    # first estimate
    set midDDTheta [expr {-sin($midTheta * $pi/180)*$scaling}]
    set lastDTheta [expr {$midDTheta + $midDDTheta}]
    set lastTheta [expr {$midTheta + ($midDTheta + $lastDTheta)/2}]
    # second estimate
    set lastDDTheta [expr {-sin($lastTheta * $pi/180)*$scaling}]
    set lastDTheta [expr {$midDTheta + ($midDDTheta + $lastDDTheta)/2}]
    set lastTheta [expr {$midTheta + ($midDTheta + $lastDTheta)/2}]
    # Now put the values back in our globals
    set dTheta $lastDTheta
    set Theta $lastTheta
}

# Run the animation by updating the physical model then the display
proc animate {} {
    global animation

    recomputeAngle
    showPendulum

    # Reschedule
    set animation [after 15 animate]
}
set animation [after 500 animate]; # Extra initial delay is visually pleasing

# Callback to handle resizing of the canvas
bind .c <Configure> {resized %w}
# Callback to stop the animation cleanly when the GUI goes away
bind .c <Destroy> {after cancel $animation}
