package require Tk
pack [label .l -text "There have been no clicks yet"]
set count 0
pack [button .b -text "click me" -command upd]
proc upd {} {
    .l configure -text "Number of clicks: [incr ::count]"
}
