package require Tcl 8.6
package require Thread

# Create the global shared state of the semaphore
set handle semaphore0
tsv::set $handle mutex [thread::mutex create]
tsv::set $handle cv [thread::cond create]
tsv::set $handle count 0
tsv::set $handle max 3

# Make five worker tasks
for {set i 0} {$i<5} {incr i} {
    lappend threads [thread::create -preserved {
	# Not bothering to wrap this in an object for demonstration
	proc init {handle} {
	    global mutex cv count max
	    set mutex [tsv::object $handle mutex]
	    set cv [tsv::object $handle cv]
	    set count [tsv::object $handle count]
	    set max [tsv::get $handle max]
	}
	proc acquire {} {
	    global mutex cv count max
	    thread::mutex lock [$mutex get]
	    while {[$count get] >= $max} {
		thread::cond wait [$cv get] [$mutex get]
	    }
	    $count incr
	    thread::mutex unlock [$mutex get]
	}
	proc release {} {
	    global mutex cv count max
	    thread::mutex lock [$mutex get]
	    if {[$count get] > 0} {
		$count incr -1
		thread::cond notify [$cv get]
	    }
	    thread::mutex unlock [$mutex get]
	}

        # The core task of the worker
	proc run {handle id} {
	    init $handle
	    acquire
	    puts "worker $id has acquired the lock"
	    after 2000
	    release
	    puts "worker $id is done"
	}

        # Wait for further instructions from the main thread
	thread::wait
    }]
}

# Start the workers doing useful work, giving each a unique id for pretty printing
set i 0
foreach t $threads {
    puts "starting thread [incr i]"
    thread::send -async $t [list run $handle $i]
}

# Wait for all the workers to finish
foreach t $threads {
    thread::release -wait $t
}
