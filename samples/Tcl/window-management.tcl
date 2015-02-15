package require Tk

# How to open a window
proc openWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        # Already existing; just reset
        wm deiconify $win
        wm state $win normal
        return
    }
    catch {destroy $win} ;# Squelch the old one
    set win [toplevel .t]
    pack [label $win.label -text "This is the window being manipulated"] \
        -fill both -expand 1
}
# How to close a window
proc closeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        destroy $win
    }
}
# How to minimize a window
proc minimizeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        wm state $win iconic
    }
}
# How to maximize a window
proc maximizeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        wm state $win zoomed
        catch {wm attribute $win -zoomed 1} ;# Hack for X11
    }
}
# How to move a window
proc moveWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        scan [wm geometry $win] "%dx%d+%d+%d" width height x y
        wm geometry $win +[incr x 10]+[incr y 10]
    }
}
# How to resize a window
proc resizeWin {} {
    global win
    if {[info exists win] && [winfo exists $win]} {
        scan [wm geometry $win] "%dx%d+%d+%d" width height x y
        wm geometry $win [incr width 10]x[incr height 10]
    }
}

grid [label .l -text "Window handle:"] [label .l2 -textvariable win]
grid [button .b1 -text "Open/Reset" -command openWin] -
grid [button .b2 -text "Close" -command closeWin] -
grid [button .b3 -text "Minimize" -command minimizeWin] -
grid [button .b4 -text "Maximize" -command maximizeWin] -
grid [button .b5 -text "Move" -command moveWin] -
grid [button .b6 -text "Resize" -command resizeWin] -
