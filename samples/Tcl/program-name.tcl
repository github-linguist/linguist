#!/usr/bin/env tclsh

proc main {args} {
    set program $::argv0
    puts "Program: $program"
}

if {$::argv0 eq [info script]} {
    main {*}$::argv
}
