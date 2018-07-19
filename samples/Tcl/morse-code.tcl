# This uses the GUI-free part of the Snack library
package require sound

# A simple pause while running the event loop, in terms of basic time units
proc pause n {
    global t
    after [expr {$t * $n}] set ok 1
    vwait ok
}
# Generate using a sine-wave filter
proc beep n {
    global frequency
    set f [snack::filter generator $frequency 30000 0.0 sine -1]
    set s [snack::sound -rate 22050]
    $s play -filter $f
    pause $n
    $s stop
    $s destroy
    $f destroy
    pause 1
}
# The dits and the dahs are just beeps of different lengths
interp alias {} dit {} beep 1
interp alias {} dah {} beep 3

set MORSE_CODE {
    "!" "---."	 "\"" ".-..-."	"$" "...-..-"	"'" ".----."
    "(" "-.--."	 ")" "-.--.-"	"+" ".-.-."	"," "--..--"
    "-" "-....-" "." ".-.-.-"	"/" "-..-."
    ":" "---..." ";" "-.-.-."	"=" "-...-"	"?" "..--.."
    "@" ".--.-." "[" "-.--."	"]" "-.--.-"	"_" "..--.-"
    "0" "-----"	 "1" ".----"	"2" "..---"	"3" "...--"
    "4" "....-"	 "5" "....."	"6" "-...."	"7" "--..."
    "8" "---.."	 "9" "----."
    "A" ".-"	 "B" "-..."	"C" "-.-."	"D" "-.."
    "E" "."	 "F" "..-."	"G" "--."	"H" "...."
    "I" ".."	 "J" ".---"	"K" "-.-"	"L" ".-.."
    "M" "--"	 "N" "-."	"O" "---"	"P" ".--."
    "Q" "--.-"	 "R" ".-."	"S" "..."	"T" "-"
    "U" "..-"	 "V" "...-"	"W" ".--"	"X" "-..-"
    "Y" "-.--"	 "Z" "--.."
}

# The code to translate text to morse code and play it
proc morse {str wpm} {
    global t MORSE_CODE
    set t [expr {1200 / $wpm}]
    # Backslash and space are special cases in various ways
    set map {"\\" {} " " {[pause 4]}}
    # Append each item in the code to the map, with an inter-letter pause after
    foreach {from to} $MORSE_CODE {lappend map $from "$to\[pause 3\]"}
    # Convert to dots and dashes
    set s [string map $map [string toupper $str]]
    # Play the dots and dashes by substituting commands for them
    subst [string map {"." [dit] "-" [dah]} $s]
    return
}

# We'll play at a fairly high pitch
set frequency 700

morse "Morse code with Tcl and Snack." 20
