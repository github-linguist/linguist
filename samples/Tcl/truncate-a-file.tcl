package require Tcl 8.5

set f [open "file" r+];	# Truncation is done on channels
chan truncate $f 1234;		# Truncate at a particular length (in bytes)
close $f
