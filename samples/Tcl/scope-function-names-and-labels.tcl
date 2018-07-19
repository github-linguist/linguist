doFoo 1 2 3;    # Will produce an error

proc doFoo {a b c} {
    puts [expr {$a + $b*$c}]
}
doFoo 1 2 3;    # Will now print 7 (and will continue to do so until doFoo is renamed or deleted
