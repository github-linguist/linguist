package require TclOO

# The class that we want to make unique instances of
set theClass Foo

# Wrong version; only a single object created
set theList [lrepeat $n [$theClass new]]

# Right version; objects distinct
set theList {}
for {set i 0} {$i<$n} {incr i} {
    lappend theList [$theClass new]
}
