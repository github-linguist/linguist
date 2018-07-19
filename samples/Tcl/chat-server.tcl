package require Tcl 8.6

# Write a message to everyone except the sender of the message
proc writeEveryoneElse {sender message} {
    dict for {who ch} $::cmap {
	if {$who ne $sender} {
	    puts $ch $message
	}
    }
}

# How to read a line (up to 256 chars long) in a coroutine
proc cgets {ch var} {
    upvar 1 $var v
    while {[gets $ch v] < 0} {
	if {[eof $ch] || [chan pending input $ch] > 256} {
	    return false
	}
	yield
    }
    return true
}

# The chatting, as seen by one user
proc chat {ch addr port} {
    ### CONNECTION CODE ###
    #Log "connection from ${addr}:${port} on channel $ch"
    fconfigure $ch -buffering none -blocking 0 -encoding utf-8
    fileevent $ch readable [info coroutine]
    global cmap
    try {

	### GET THE NICKNAME OF THE USER ###
	puts -nonewline $ch "Please enter your name: "
	if {![cgets $ch name]} {
	    return
	}
	#Log "Mapping ${addr}:${port} to ${name} on channel $ch"
	dict set cmap $name $ch
	writeEveryoneElse $name "+++ $name arrived +++"

	### MAIN CHAT LOOP ###
	while {[cgets $ch line]} {
	    writeEveryoneElse $name "$name> $line"
	}

    } finally {
	### DISCONNECTION CODE ###
	if {[info exists name]} {
	    writeEveryoneElse $name "--- $name left ---"
	    dict unset cmap $name
	}
	close $ch
	#Log "disconnection from ${addr}:${port} on channel $ch"
    }
}

# Service the socket by making corouines running [chat]
socket -server {coroutine c[incr count] chat} 4004
set ::cmap {};		# Dictionary mapping nicks to channels
vwait forever;		# Run event loop
