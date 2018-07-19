#****f* RosettaCode/TclDocDemo
# FUNCTION
#    TclDocDemo is a simple illustration of how to do documentation
#    of Tcl code using Robodoc.
# SYNOPSYS
#    TclDocDemo foo bar
# INPUTS
#    foo -- the first part of the message to print
#    bar -- the last part of the message to print
# RESULT
#    No result
# NOTES
#    Prints a message based on a template by filling in with the
#    supplied strings.
#*****
proc TclDocDemo {foo bar} {
    puts [format "%s -- %s" $foo $bar]
}
