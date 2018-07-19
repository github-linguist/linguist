package require Thread
package require Tk

# Make the shared state
canvas .c		;# So we can allocate the display lines in one loop
set m [thread::mutex create]
for {set i 0} {$i<100} {incr i} {
    set bucket b$i	;# A handle for every bucket...
    tsv::set buckets $bucket 50
    lappend buckets $bucket
    lappend lines [.c create line 0 0 0 0]
}
tsv::set still going 1

# Make the "make more equal" task
lappend tasks [thread::create {
    # Perform an atomic update of two cells
    proc transfer {b1 b2 val} {
	variable m
	thread::mutex lock $m
	set v [tsv::get buckets $b1]
	if {$val > $v} {
	    set val $v
	}
	tsv::incr buckets $b1 [expr {-$val}]
	tsv::incr buckets $b2 $val
	thread::mutex unlock $m
    }

    # The task itself; we loop this round frequently
    proc task {mutex buckets} {
	variable m $mutex b $buckets i 0
	while {[tsv::get still going]} {
	    set b1 [lindex $b $i]
	    if {[incr i] == [llength $b]} {set i 0}
	    set b2 [lindex $b $i]

	    if {[tsv::get buckets $b1] > [tsv::get buckets $b2]} {
		transfer $b1 $b2 1
	    } else {
		transfer $b1 $b2 -1
	    }
	}
    }
    thread::wait
}]

# Make the "mess things up" task
lappend tasks [thread::create {
    # Utility to pick a random item from a list
    proc pick list {
	lindex $list [expr {int(rand() * [llength $list])}]
    }
    proc transfer {b1 b2 val} {
	variable m
	thread::mutex lock $m
	set v [tsv::get buckets $b1]
	if {$val > $v} {
	    set val $v
	}
	tsv::incr buckets $b1 [expr {-$val}]
	tsv::incr buckets $b2 $val
	thread::mutex unlock $m
    }

    # The task to move a large amount between two random buckets
    proc task {mutex buckets} {
	variable m $mutex b $buckets
	while {[tsv::get still going]} {
	    set b1 [pick $b]
	    set b2 [pick $b]
	    transfer $b1 $b2 [expr {[tsv::get buckets $b1] / 3}]
	}
    }
    thread::wait
}]

# The "main" task; we keep GUI operations in the main thread
proc redisplay {} {
    global m buckets lines
    thread::mutex lock $m
    set i 1
    foreach b $buckets l $lines {
	.c coords $l $i 0 $i [tsv::get buckets $b]
	incr i 2
    }
    thread::mutex unlock $m
    after 100 redisplay
}

# Start tasks and display
.c configure -width 201 -height 120
pack .c
redisplay
foreach t $tasks {
    thread::send -async $t [list task $m $buckets]
}

# Wait for user to close window, then tidy up
tkwait window .
tsv::set still going 0
thread::broadcast thread::exit
