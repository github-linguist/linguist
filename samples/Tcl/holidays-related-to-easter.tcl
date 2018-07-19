package require Tcl 8.5; # Advanced date handling engine

# Easter computation code from http://www.assa.org.au/edm.html
proc EasterDate year {
    set FirstDig [expr {$year / 100}]
    set Remain19 [expr {$year % 19}]

    # calculate Paschal Full Moon date
    set temp [expr {($FirstDig - 15)/2 + 202 - 11*$Remain19}]
    if {$FirstDig in {21 24 25 27 28 29 30 31 32 34 35 38}} {
	incr temp -1
    } elseif {$FirstDig in {33 36 37 39 40}} {
	incr temp -2
    }
    set temp [expr {$temp % 30}]

    set tA [expr {$temp + 21}]
    if {$temp == 29} {incr tA -1}
    if {$temp == 28 && $Remain19 > 10} {incr tA -1}

    # find the next Sunday
    set tB [expr {($tA - 19) % 7}]

    set tC [expr {(40 - $FirstDig) % 4}]
    if {$tC == 3} {incr tC}
    if {$tC > 1} {incr tC}

    set temp [expr {$year % 100}]
    set tD [expr {($temp + $temp/4) % 7}]

    set tE [expr {((20 - $tB - $tC - $tD) % 7) + 1}]
    set d [expr {$tA + $tE}]

    # return the date
    if {$d > 31} {
	return [format "%02d April %04d" [expr {$d - 31}] $year]
    } else {
	return [format "%02d March %04d" $d $year]
    }
}

# Use the Easter calculator to work out the data for the feasts
proc DateInfo year {
    set fields [format %4d: $year]\t
    set easter [clock scan [EasterDate $year] -format "%d %B %Y"]
    foreach {name delta} {
	Easter     0
	Ascension 39
	Pentecost 49
	Trinity   56
	Corpus    60
    } {
	set when [clock add $easter $delta days]
	append fields [clock format $when -format "${name}: %d %b,  "]
    }
    return [string trimright $fields " ,"]
}

# Print the required info
puts "Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:"
for {set year 400} {$year <= 2100} {incr year 100} {
    puts [DateInfo $year]
}
puts ""
puts "Christian holidays, related to Easter, for years from 2010 to 2020 CE:"
for {set year 2010} {$year <= 2020} {incr year} {
    puts [DateInfo $year]
}
