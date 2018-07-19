package require Tcl 8.5;   # arbitrary precision integers; we can count until we run out of memory!
while 1 {
    puts [format "%llo" [incr counter]]
}
