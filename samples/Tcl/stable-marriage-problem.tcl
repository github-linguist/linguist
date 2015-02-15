package require Tcl 8.5

# Functions as aliases to standard commands
interp alias {} tcl::mathfunc::pos {} ::lsearch -exact
interp alias {} tcl::mathfunc::nonempty {} ::llength

# The stability check
proc check engaged {
    global preferences
    set inverse [lreverse $engaged]
    set errmsg "%s and %s like each other better than their present partners,\
	    %s and %s respectively"
    dict for {she he} $engaged {
	set shelikes [dict get $preferences $she]
	set shelikesbetter [lrange $shelikes 0 [expr {pos($shelikes,$he)}]]
	set helikes [dict get $preferences $he]
	set helikesbetter [lrange $helikes 0 [expr {pos($helikes,$she)}]]
	foreach guy $shelikesbetter {
	    set guysgirl [dict get $inverse $guy]
	    set guylikes [dict get $preferences $guy]
	    if {pos($guylikes,$guysgirl) > pos($guylikes,$she)} {
		puts [format $errmsg $she $guy $he $guysgirl]
		return 0
	    }
	}
	foreach gal $helikesbetter {
	    set galsguy [dict get $engaged $gal]
	    set gallikes [dict get $preferences $gal]
	    if {pos($gallikes,$galsguy) > pos($gallikes,$he)} {
		puts [format $errmsg $he $gal $she $galsguy]
		return 0
	    }
	}
    }
    return 1
}

# The match-making algorithm
proc matchmaker {} {
    global guys gals preferences
    set guysfree $guys
    set engaged {}
    array set p $preferences
    while {nonempty($guysfree)} {
	set guysfree [lassign $guysfree guy]
	set p($guy) [set guyslist [lassign $p($guy) gal]]
	if {![dict exists $engaged $gal]} {
	    # She's free
	    dict set engaged $gal $guy
	    puts "  $guy and $gal"
	    continue
	}
	# The bounder proposes to an engaged lass!
	set fiance [dict get $engaged $gal]
	if {pos($p($gal), $fiance) > pos($p($gal), $guy)} {
	    # She prefers the new guy
	    dict set engaged $gal $guy
	    puts "  $gal dumped $fiance for $guy"
	    set guy $fiance
	}
	if {nonempty($p($guy))} {
	    lappend guysfree $guy
	}
    }
    return $engaged
}

# Problem dataset; preferences unified since all names distinct
set guys {abe bob col  dan ed  fred gav hal  ian jon}
set gals {abi bea cath dee eve fay  gay hope ivy jan}
set preferences {
    abe  {abi  eve  cath ivy  jan  dee  fay  bea  hope gay}
    bob  {cath hope abi  dee  eve  fay  bea  jan  ivy  gay}
    col  {hope eve  abi  dee  bea  fay  ivy  gay  cath jan}
    dan  {ivy  fay  dee  gay  hope eve  jan  bea  cath abi}
    ed   {jan  dee  bea  cath fay  eve  abi  ivy  hope gay}
    fred {bea  abi  dee  gay  eve  ivy  cath jan  hope fay}
    gav  {gay  eve  ivy  bea  cath abi  dee  hope jan  fay}
    hal  {abi  eve  hope fay  ivy  cath jan  bea  gay  dee}
    ian  {hope cath dee  gay  bea  abi  fay  ivy  jan  eve}
    jon  {abi  fay  jan  gay  eve  bea  dee  cath ivy  hope}

    abi  {bob  fred jon  gav  ian  abe dan  ed  col  hal}
    bea  {bob  abe  col  fred gav  dan ian  ed  jon  hal}
    cath {fred bob  ed   gav  hal  col ian  abe dan  jon}
    dee  {fred jon  col  abe  ian  hal gav  dan bob  ed}
    eve  {jon  hal  fred dan  abe  gav col  ed  ian  bob}
    fay  {bob  abe  ed   ian  jon  dan fred gav col  hal}
    gay  {jon  gav  hal  fred bob  abe col  ed  dan  ian}
    hope {gav  jon  bob  abe  ian  dan hal  ed  col  fred}
    ivy  {ian  col  hal  gav  fred bob abe  ed  jon  dan}
    jan  {ed   hal  gav  abe  bob  jon col  ian fred dan}
}

# The demonstration code
puts "Engagements:"
set engaged [matchmaker]

puts "\nCouples:"
set pfx ""
foreach gal $gals {
    puts -nonewline "$pfx  $gal is engaged to [dict get $engaged $gal]"
    set pfx ",\n"
}
puts "\n"
puts "Engagement stability check [lindex {FAILED PASSED} [check $engaged]]"

puts "\n\nSwapping two fiances to introduce an error"
set tmp [dict get $engaged [lindex $gals 0]]
dict set engaged [lindex $gals 0] [dict get $engaged [lindex $gals 1]]
dict set engaged [lindex $gals 1] $tmp
foreach gal [lrange $gals 0 1] {
    puts "  $gal is now engaged to [dict get $engaged $gal]"
}
puts ""
puts "Engagement stability check [lindex {FAILED PASSED} [check $engaged]]"
