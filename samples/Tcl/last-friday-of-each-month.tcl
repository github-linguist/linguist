package require Tcl 8.5
set year [lindex $argv 0]
foreach dm {02/1 03/1 04/1 05/1 06/1 07/1 08/1 09/1 10/1 11/1 12/1 12/32} {
    # The [clock scan] code is unhealthily clever; use it for our own evil purposes
    set t [clock scan "last friday" -base [clock scan $dm/$year -gmt 1] -gmt 1]
    # Print the interesting part
    puts [clock format $t -format "%Y-%m-%d" -gmt 1]
}
