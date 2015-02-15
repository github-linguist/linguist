proc readConfig {filename {defaults {}}} {
    global cfg
    # Read the file in
    set f [open $filename]
    set contents [read $f]
    close $f
    # Set up the defaults, if supplied
    foreach {var defaultValue} $defaults {
	set cfg($var) $defaultValue
    }
    # Parse the file's contents
    foreach line [split $contents "\n"] {
	set line [string trim $line]
	# Skip comments
	if {[string match "#*" $line] || [string match ";*" $line]} continue
	# Skip blanks
	if {$line eq ""} continue

	if {[regexp {^\w+$} $line]} {
	    # Boolean case
	    set cfg([string tolower $line]) true
	} elseif {[regexp {^(\w+)\s+([^,]+)$} $line -> var value]} {
	    # Simple value case
	    set cfg([string tolower $var]) $value
	} elseif {[regexp {^(\w+)\s+(.+)$} $line -> var listValue]} {
	    # List value case
	    set cfg([string tolower $var]) {}
	    foreach value [split $listValue ","] {
		lappend cfg([string tolower $var]) [string trim $value]
	    }
	} else {
	    error "malformatted config file: $filename"
	}
    }
}

# Need to supply some default values due to config file grammar ambiguities
readConfig "fruit.cfg" {
    needspeeling false
    seedsremoved false
}
puts "Full name: $cfg(fullname)"
puts "Favourite: $cfg(favouritefruit)"
puts "Peeling?   $cfg(needspeeling)"
puts "Unseeded?  $cfg(seedsremoved)"
puts "Family:    $cfg(otherfamily)"
