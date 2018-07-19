package require Tk 8.5
set curWindow [lindex [wm stackorder .] end]
# Everything below will work with anything from Tk 8.0 onwards
set x [expr {[winfo pointerx .] - [winfo rootx $curWindow]}]
set y [expr {[winfo pointery .] - [winfo rooty $curWindow]}]
tk_messageBox -message "the mouse is at ($x,$y) in window $curWindow"
