package require Tk
package require tcl3d

proc resizedWin {win w h} {
    glViewport 0 0 $w $h
    glMatrixMode GL_PROJECTION
    glLoadIdentity
    glOrtho -30.0 30.0 -30.0 30.0 -30.0 30.0
    glMatrixMode GL_MODELVIEW
}
proc paintShape {win} {
    glClearColor 0.0 0.0 0.0 0.5
    glClear [expr {$::GL_COLOR_BUFFER_BIT+$::GL_DEPTH_BUFFER_BIT}]
    glShadeModel GL_SMOOTH
    glLoadIdentity
    glTranslatef -15.0 -15.0 0.0
    glBegin GL_TRIANGLES
    glColor3f 1.0 0.0 0.0
    glVertex2f 5.0 5.0
    glColor3f 0.0 1.0 0.0
    glVertex2f 25.0 5.0
    glColor3f 0.0 0.0 1.0
    glVertex2f 5.0 25.0
    glEnd
    $win swapbuffers
}

togl .surface -width 640 -height 480 -double true -depth true \
    -displayproc paintShape -reshapeproc resizedWin
pack .surface -fill both -expand 1
