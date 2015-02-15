package require Tcl 8.5

# Throw
proc e {args} {
    error "error message" "error message for stack trace" {errorCode list}
}

# Catch and rethrow
proc f {} {
    if {[catch {e 1 2 3 4} errMsg options] != 0} {
        return -options $options $errMsg
    }
}

f
