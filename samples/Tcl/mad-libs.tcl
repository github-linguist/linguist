package require Tcl 8.5

# Read the template...
puts [string repeat "-" 70]
puts "Enter the story template, ending with a blank line"
while {[gets stdin line] > 0} {
    append content $line "\n"
}

# Read the mapping...
puts [string repeat "-" 70]
set mapping {}
foreach piece [regexp -all -inline {<[^>]+>} $content] {
    if {[dict exists $mapping $piece]} continue
    puts -nonewline "Give me a $piece: "
    flush stdout
    dict set mapping $piece [gets stdin]
}

# Apply the mapping and print...
puts [string repeat "-" 70]
puts -nonewline [string map $mapping $content]
puts [string repeat "-" 70]
