package require Tcl 8.5

# How to process a single value, adding it to the table mapping stems to
# leaves.
proc addSLValue {tblName value {splitFactor 10}} {
    upvar 1 $tblName tbl
    # Extract the stem and leaf
    if {$value < 0} {
	set value [expr {round(-$value)}]
	set stem -[expr {$value / $splitFactor}]
    } else {
	set value [expr {round($value)}]
	set stem [expr {$value / $splitFactor}]
    }
    if {![info exist tbl]} {
	dict set tbl min $stem
    }
    dict set tbl max $stem
    set leaf [expr {$value % $splitFactor}]
    dict lappend tbl $stem $leaf
}

# How to do the actual output of the stem-and-leaf table, given that we have
# already done the splitting into stems and leaves.
proc printSLTable {tblName} {
    upvar 1 $tblName tbl
    # Get the range of stems
    set min [dict get $tbl min]
    set max [dict get $tbl max]
    # Work out how much width the stems take so everything lines up
    set l [expr {max([string length $min], [string length $max])}]
    # Print out the table
    for {set i $min} {$i <= $max} {incr i} {
	if {![dict exist $tbl $i]} {
	    puts [format " %*d |"    $l $i]
	} else {
	    puts [format " %*d | %s" $l $i [dict get $tbl $i]]
	}
    }
}

# Assemble the parts into a full stem-and-leaf table printer.
proc printStemLeaf {dataList {splitFactor 10}} {
    foreach value [lsort -real $dataList] {
	addSLValue tbl $value $splitFactor
    }
    printSLTable tbl
}

# Demo code
set data {
    12  127 28  42  39  113 42  18  44  118 44  37  113 124 37  48  127 36
    29  31  125 139 131 115 105 132 104 123 35  113 122 42  117 119 58  109
    23  105 63  27  44  105 99  41  128 121 116 125 32  61  37  127 29  113
    121 58  114 126 53  114 96  25  109 7   31  141 46  13  27  43  117 116
    27  7   68  40  31  115 124 42  128 52  71  118 117 38  27  106 33  117
    116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27  18  28
    48  125 107 114 34  133 45  120 30  127 31  116 146
}
printStemLeaf $data
