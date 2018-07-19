package require TclOO
# First create a simple, conventional class and object
oo::class create Example {
    method foo {} {
        puts "this is foo"
    }
    method bar {} {
        puts "this is bar"
    }
}
Example create example

# Modify the object to have a custom ‘unknown method’ interceptor
oo::objdefine example {
    method unknown {name args} {
        puts "tried to handle unknown method \"$name\""
        if {[llength $args]} {
            puts "it had arguments: $args"
        }
    }
}

# Show off what we can now do...
example foo;       # prints “this is foo”
example bar;       # prints “this is bar”
example grill;     # prints “tried to handle unknown method "grill"”
example ding dong; # prints “tried to handle unknown method "ding"”
                   # prints “it had arguments: dong”
