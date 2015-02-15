package require Tcl 8.6
package require Thread

# Really ought to go in a package
eval [set rendezvousEngine {
array set Select {w {} c 0}

# Turns the task into a coroutine, making it easier to write in "Ada style".
# The real thread ids are stored in shared variables.
proc task {id script} {
    global rendezvousEngine
    set task [list coroutine RTask eval "$script;thread::exit"]
    tsv::set tasks $id [thread::create \
		"$rendezvousEngine;$task;thread::wait"]
}

# A simple yielding pause.
proc pause t {
    after $t [info coroutine]
    yield
}

# Wait for a message. Note that this is *not* pretty code and doesn't do
# everything that the Ada rendezvous does.
proc select args {
    global Select
    set var [namespace which -variable Select](m[incr Select(c)])
    set messages {}
    foreach {message vars body} $args {
	dict set messages $message $body
	dict set bindings $message $vars
    }
    lappend Select(w) [list $var [dict keys $messages]]
    try {
	set Master ""
	while {$Master eq ""} {
	    set Master [yield]
	}
	lassign $Master message responder payload
	foreach vbl [dict get $bindings $message] value $payload {
	    upvar 1 $vbl v
	    set v $value
	}
	set body [dict get $messages $message]
	set code [uplevel 1 [list catch $body ::Select(em) ::Select(op)]]
	set opts $Select(op)
	if {$code == 1} {
	    dict append opts -errorinfo \
		"\n    while processing message\n$message $payload"
	}
	set $responder [list $code $Select(em) $opts]
    } finally {
	catch {unset $var}
	set Select(w) [lrange $Select(w) 0 end-1]
    }
}

# This acts as a receiver for messages, feeding them into the waiting
# [select].  It is incomplete as it should (but doesn't) queue messages that
# can't be received currently.
proc receive {message args} {
    global Select
    lassign [lindex $Select(w) end] var messages
    if {$message ni $messages} {
	throw BAD_MESSAGE "don't know message $message"
    }
    set responder [namespace which -variable Select](r[incr Select(c)])
    set $responder ""
    RTask [list $message $responder $args]
    set response [set $responder]
    unset responder
    after 1
    return $response
}

# This dispatches a message to a task in another thread.
proc send {target message args} {
    after 1
    set t [tsv::get tasks $target]
    if {![thread::send $t [list receive $message {*}$args] response]} {
	lassign $response code msg opts
	return -options $opts $msg
    } else {
	return -code error $response
    }
}
}]

# The backup printer task.
task BackupPrinter {
    set n 5
    while {$n >= 0} {
	select Print msg {
	    if {$n > 0} {
		incr n -1
		puts Backup:$msg
	    } else {
		throw OUT_OF_INK "out of ink"
	    }
	}
    }
}

# The main printer task.
task MainPrinter {
    set n 5
    set Backup BackupPrinter
    while 1 {
	select Print msg {
	    try {
		if {$n > 0} {
		    incr n -1
		    puts Main:$msg
		} elseif {$Backup ne ""} {
		    send $Backup Print $msg
		} else {
		    throw OUT_OF_INK "out of ink"
		}
	    } trap OUT_OF_INK {} {
		set Backup ""
		throw OUT_OF_INK "out of ink"
	    }
	}
    }
}

# Tasks that generate messages to print.
task HumptyDumpty {
    pause 100
    try {
	send MainPrinter Print "Humpty Dumpty sat on a wall."
	send MainPrinter Print "Humpty Dumpty had a great fall."
	send MainPrinter Print "All the King's horses and all the King's men"
	send MainPrinter Print "Couldn't put Humpty together again."
    } trap OUT_OF_INK {} {
	puts "Humpty Dumpty out of ink!"
    }
}
task MotherGoose {
    pause 100
    try {
	send MainPrinter Print "Old Mother Goose"
	send MainPrinter Print "When she wanted to wander,"
	send MainPrinter Print "Would ride through the air"
	send MainPrinter Print "On a very fine gander."
	send MainPrinter Print "Jack's mother came in,"
	send MainPrinter Print "And caught the goose soon,"
	send MainPrinter Print "And mounting its back,"
	send MainPrinter Print "Flew up to the moon."
    } trap OUT_OF_INK {} {
	puts "Mother Goose out of ink!"
    }
}

# Wait enough time for the example to run and then finish
after 1000
thread::broadcast thread::exit
