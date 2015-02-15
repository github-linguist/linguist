# Make ourselves a very simple templating lib; just two commands
proc TAG {name args} {
    set body [lindex $args end]
    set result "<$name"
    foreach {t v} [lrange $args 0 end-1] {
	append result " $t=\"" $v "\""
    }
    append result ">" [string trim [uplevel 1 [list subst $body]]] "</$name>"
}
proc FOREACH {var lst str} {
    upvar 1 $var v
    set result {}
    set s [list subst $str]
    foreach v $lst {append result [string trim [uplevel 1 $s]]}
    return $result
}

# Build the data we're displaying
set titles {"" "X" "Y" "Z"}
set data {}
for {set x 0} {$x < 4} {incr x} {
    # Inspired by the Go solution, but with extra arbitrary digits to show 4-char wide values
    lappend data [list \
	    [expr {$x+1}] [expr {$x*3010}] [expr {$x*3+1298}] [expr {$x*2579+2182}]]
}

# Write the table to standard out
puts [TAG table border 1 {
    [TAG tr bgcolor #f0f0f0 {
	[FOREACH head $titles {
	    [TAG th {$head}]
	}]
    }]
    [FOREACH row $data {
	[TAG tr bgcolor #ffffff {
	    [FOREACH col $row {
		[TAG td align right {$col}]
	    }]
	}]
    }]
}]
