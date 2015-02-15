package require Tcl 8.5

set text {Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.}

array set max {}
foreach line [split $text \n] {
    set col 0
    set thisline [split $line \$]
    lappend words $thisline
    foreach word $thisline {
        set max([incr col]) [expr {[info exists max($col)]
                                    ? max($max($col), [string length $word])
                                    : [string length $word]
                            }]
    }
}

proc justify {word position width} {
    switch -exact -- $position {
        left {
            return [format "%-*s" $width $word]
        }
        center {
            set lpadw [expr {($width - [string length $word])/2}]
            return [format "%s%-*s" [string repeat " " $lpadw] [incr width -$lpadw] $word]
        }
        right {
            return [format "%*s" $width $word]
        }
    }
}

foreach position {left center right} {
    foreach thisline $words {
        set col 0
        set line ""
        foreach word $thisline {
            append line [justify $word $position $max([incr col])] " "
        }
        puts [string trimright $line]
    }
    puts ""
}
