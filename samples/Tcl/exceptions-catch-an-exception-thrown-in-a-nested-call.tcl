package require Tcl 8.5

proc foo {} {
    set code [catch {bar} ex options]
    if {$code == 1} {
        switch -exact -- $ex {
            U0      {puts "caught exception U0"}
            default {return -options $options $ex ;# re-raise exception}
        }
    }
}

proc bar {} {baz}

# create an alias to pass the initial exception U0 to the baz proc
interp alias {} baz {} _baz U0

proc _baz {exception} {
    # re-set the alias so subsequent invocations will use exception U1
    interp alias {} baz {} _baz U1
    # throw
    return -code error $exception
}

foo
foo
