package require Tk

# Model
set field 0

# View
place [ttk::frame .bg] -relwidth 1 -relheight 1; # Hack to make things look nice
pack [ttk::labelframe .val -text "Value"]
pack [ttk::entry .val.ue -textvariable field \
	-validate key -invalidcommand bell \
	-validatecommand {string is integer %P}]
pack [ttk::button .inc -text "increment" -command up]
pack [ttk::button .dec -text "decrement" -command down]

# Controller
proc up {} {
    global field
    incr field
}
proc down {} {
    global field
    incr field -1
}
# Attach this controller to the Model; easier than manual calling
trace add variable field write updateEnables
proc updateEnables {args} {
    global field
    .inc state [expr {$field < 10 ? "!disabled" : "disabled"}]
    .dec state [expr {$field > 0 ? "!disabled" : "disabled"}]
}
updateEnables; # Force initial state of buttons
