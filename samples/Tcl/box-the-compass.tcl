proc angle2compass {angle} {
    set dirs {
	N NbE N-NE NEbN NE NEbE E-NE EbN E EbS E-SE SEbE SE SEbS S-SE SbE
	S SbW S-SW SWbS SW SWbW W-SW WbS W WbN W-NW NWbW NW NWbN N-NW NbW
    }
    set unpack {N "north" E "east" W "west" S "south" b " by "}

    # Compute the width of each compass segment
    set sep [expr {360.0 / [llength $dirs]}]

    # Work out which segment contains the compass angle
    set dir [expr {round((fmod($angle + $sep/2, 360) - $sep/2) / $sep)}]

    # Convert to a named direction, capitalized as in the wikipedia article
    return [string totitle [string map $unpack [lindex $dirs $dir]]]
}

# Box the compass, using the variable generation algorithm described
for {set i 0} {$i < 33} {incr i} {
    set heading [expr {$i * 11.25}]
    if {$i % 3 == 1} {set heading [expr {$heading + 5.62}]}
    if {$i % 3 == 2} {set heading [expr {$heading - 5.62}]}
    set index [expr {$i % 32 + 1}]

    # Pretty-print the results of converting an angle to a compass heading
    puts [format "%2i %-18s %7.2f°" $index [angle2compass $heading] $heading]
}
