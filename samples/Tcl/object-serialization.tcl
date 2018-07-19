package require Tcl 8.6
package require TclOO::serializer 0.1

# These classes are inspired by the Perl example
oo::class create Greeting {
    superclass oo::serializable
    variable v
    constructor {} {
        set v "Hello world!"
    }
    method get {} {
        return $v
    }
}
oo::class create SubGreeting {
    superclass Greeting oo::serializable
    variable v
    constructor {} {
        set v "Hello world from Junior!"
    }
}
oo::class create GreetingsHolder {
    superclass oo::serializable
    variable o1 o2
    constructor {greeting1 greeting2} {
        set o1 $greeting1
        set o2 $greeting2
    }
    method printGreetings {} {
        puts [$o1 get]
        puts [$o2 get]
    }
    destructor {
        $o1 destroy
        $o2 destroy
    }
}

# Make some objects and store them
GreetingsHolder create holder [Greeting new] [SubGreeting new]
set f [open "objects.dat" w]
puts $f [oo::serialize holder]
close $f

# Delete the objects
holder destroy

# Recreate the objects from the file and show that they work
set f [open "objects.dat" r]
set obj [oo::deserialize [read $f]]
close $f
$obj printGreetings
