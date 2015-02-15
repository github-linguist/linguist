package require sound

# Helper to do a responsive wait
proc delay t {after $t {set ::doneDelay ok}; vwait ::doneDelay}

# Make an in-memory recording object
set recording [snack::sound -encoding "Lin16" -rate 44100 -channels 1]

# Set it doing the recording, wait for a second, and stop
$recording record -append true
delay 1000
$recording stop

# Convert the internal buffer to viewable numbers, and print them out
binary scan [$recording data -byteorder littleEndian] s* words
puts [join $words ", "]

# Destroy the recording object
$recording destroy
