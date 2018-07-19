package require Thread

foreach name {Aristotle Kant Spinoza Marx Russel} {
    lappend forks [thread::mutex create]
    lappend tasks [set t [thread::create -preserved {
        # Implement each task as a coroutine internally for simplicity of presentation
        # This is because we want to remain able to receive messages so we can shut
        # down neatly at the end of the program.
	interp alias {} doTask {} coroutine t philosopher
	proc delay {expression} {
	    yield [after [expr $expression] [info coroutine]]
	}

        # Forks are mutexes...
        proc pickUpFork fork {
            thread::mutex lock $fork
        }
        proc putDownFork fork {
            thread::mutex unlock $fork
        }

        # The actual implementation of the task
	proc philosopher {f1 f2} {
	    global name
	    # Always acquire forks in order; prevents deadlock
            # Uses the "natural" order of the lexicographical order of the fork names
	    if {$f1 > $f2} {
                lassign [list $f1 $f2] f2 f1
            }

            # The classic "philosophers" loop
	    while {true} {
		puts "$name is thinking"
		delay {int(200*rand())}

		puts "$name is hungry, getting fork in left hand"
		pickUpFork $f1
		delay {int(2000*rand())} ;# Make deadlock likely if it is possible!

		puts "$name is hungry, getting fork in right hand"
		pickUpFork $f2

		puts "$name is eating"
		delay {int(2000*rand())}

		puts "$name has finished eating; putting down forks"
		putDownFork $f2
		putDownFork $f1
                delay 100
	    }
	}
	thread::wait
    }]]
    thread::send $t [list set name $name]
}

# Set the tasks going
foreach t $tasks {f1 f2} {0 1 1 2 2 3 3 4 4 0} {
    thread::send -async $t [list \
            doTask [lindex $forks $f1] [lindex $forks $f2]]
}

# Kill everything off after 30 seconds; that's enough for demonstration!
after 30000
puts "Completing..."
foreach t $tasks {
    thread::send -async $t thread::exit
}
