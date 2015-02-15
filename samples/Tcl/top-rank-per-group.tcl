package require Tcl 8.5

set text {Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190}

set data [dict create]
foreach line [split $text \n] {
    lassign [split $line ,] name id salary dept
    dict lappend data $dept [list $name $id $salary]
}

proc top_n_salaries {n data} {
    incr n -1
    dict for {dept employees} $data {
        puts "Department $dept"
        foreach emp [lrange [lsort -integer -decreasing -index 2 $employees] 0 $n] {
            puts [format "   %-20s %-8s %8d" {*}$emp]
        }
        puts ""
    }
}

top_n_salaries 3 $data
