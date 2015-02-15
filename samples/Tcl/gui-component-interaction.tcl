package require Tk

###--- Our data Model! ---###
# A single variable will do just fine
set field 0

###--- Lay out the GUI components in our View ---###
# We use the Ttk widget set here; it looks much better on Windows and OSX

# First, a quick hack to make things look even nicer
place [ttk::frame .bg] -relwidth 1 -relheight 1

# A labelled frame containing an entry field constrained to use numbers
pack [ttk::labelframe .val -text "Value"]
pack [ttk::entry .val.ue -textvariable field \
	-validate key -invalidcommand bell \
	-validatecommand {string is integer %P}]
# Now, a pair of buttons
pack [ttk::button .inc -text "increment" -command step]
pack [ttk::button .rnd -text "random" -command random]

###--- Now we define the behaviors, the Controller ---###
# How to respond to a click on the "increment" button
proc step {} {
    global field
    incr field
}
# How to respond to a click on the "random" button
proc random {} {
    global field
    if {[tk_messageBox -type yesno -parent . \
	    -message "Reset to random?"] eq "yes"} {
	set field [expr {int(rand() * 5000)}]
    }
}
