###############################################################################
# measure and plot times
package require Tk
package require struct::list
namespace path ::tcl::mathfunc

proc create_log10_plot {title xlabel ylabel xs ys labels shapes colours} {
    set w [toplevel .[clock clicks]]
    wm title $w $title
    pack [canvas $w.c -background white]
    pack [canvas $w.legend -background white]
    update
    plot_log10 $w.c $w.legend $title $xlabel $ylabel $xs $ys $labels $shapes $colours
    $w.c config -scrollregion [$w.c bbox all]
    update
}

proc plot_log10 {canvas legend title xlabel ylabel xs ys labels shapes colours} {
    global xfac yfac
    set log10_xs [map {_ {log10 $_}} $xs]
    foreach series $ys {
        lappend log10_ys [map {_ {log10 $_}} $series]
    }
    set maxx [max {*}$log10_xs]
    set yvalues [lsort -real [struct::list flatten $log10_ys]]
    set firstInf [lsearch $yvalues Inf]
    set maxy [lindex $yvalues [expr {$firstInf == -1 ? [llength $yvalues] - 1 : $firstInf - 1}]]

    set xfac [expr {[winfo width $canvas] * 0.8/$maxx}]
    set yfac [expr {[winfo height $canvas] * 0.8/$maxy}]

    scale $canvas x 0 $maxx $xfac "log10($xlabel)"
    scale $canvas y 0 $maxy $yfac "log10($ylabel)" $maxx $xfac

    $legend create text 30 0 -text $title -anchor nw
    set count 1
    foreach series $log10_ys shape $shapes colour $colours label $labels {
        plotxy $canvas $log10_xs $series $shape $colour
        legenditem $legend [incr count] $shape $colour $label
    }
}

proc map {lambda list} {
    set res [list]
    foreach item $list {lappend res [apply $lambda $item]}
    return $res
}

proc legenditem {legend pos shape colour label} {
    set y [expr {$pos * 15}]
    $shape $legend 20 $y -fill $colour
    $legend create text 30 $y -text $label -anchor w
}

# The actual plotting engine
proc plotxy {canvas _xs _ys shape colour} {
    global xfac yfac
    foreach x $_xs y $_ys {
        if {$y < Inf} {
            lappend xs $x
            lappend ys $y
        }
    }
    set coords [list]
    foreach x $xs y $ys {
        set coord_x [expr {$x*$xfac}]
        set coord_y [expr {-$y*$yfac}]
        $shape $canvas $coord_x $coord_y -fill $colour
        lappend coords $coord_x $coord_y
    }
    $canvas create line $coords -smooth true
}
# Rescales the contents of the given canvas
proc scale {canvas direction from to fac label {other_to 0} {other_fac 0}} {
    set f [expr {$from*$fac}]
    set t [expr {$to*$fac}]
    switch -- $direction {
        x {
            set f [expr {$from * $fac}]
            set t [expr {$to * $fac}]
            # create x-axis
            $canvas create line $f 0 $t 0
            $canvas create text $f 0 -anchor nw -text $from
            $canvas create text $t 0 -anchor n -text [format "%.1f" $to]
            $canvas create text [expr {($f+$t)/2}] 0 -anchor n -text $label

        }
        y {
            set f [expr {$from * -$fac}]
            set t [expr {$to * -$fac}]
            # create y-axis
            $canvas create line 0 $f 0 $t
            $canvas create text 0 $f -anchor se -text $from
            $canvas create text 0 $t -anchor e -text [format "%.1f" $to]
            $canvas create text 0 [expr {($f+$t)/2}] -anchor e -text $label
            # create gridlines
            set xmax [expr {$other_to * $other_fac}]
            for {set i 1} {$i < $to} {incr i} {
                set y [expr {$i * -$fac}]
                $canvas create line 0 $y $xmax $y -dash .
            }
        }
    }
}
# Helper to make points, which are otherwise not a native item type
proc dot {canvas x y args} {
    set id [$canvas create oval [expr {$x-3}] [expr {$y-3}] \
                [expr {$x+3}] [expr {$y+3}]]
    $canvas itemconfigure $id {*}$args
}
proc square {canvas x y args} {
    set id [$canvas create rectangle [expr {$x-3}] [expr {$y-3}] \
                [expr {$x+3}] [expr {$y+3}]]
    $canvas itemconfigure $id {*}$args
}
proc cross {canvas x y args} {
    set l1 [$canvas create line [expr {$x-3}] $y [expr {$x+3}] $y]
    set l2 [$canvas create line $x [expr {$y-3}] $x [expr {$y+3}]]
    $canvas itemconfigure $l1 {*}$args
    $canvas itemconfigure $l2 {*}$args
}
proc x {canvas x y args} {
    set l1 [$canvas create line [expr {$x-3}] [expr {$y-3}] [expr {$x+3}] [expr {$y+3}]]
    set l2 [$canvas create line [expr {$x+3}] [expr {$y-3}] [expr {$x-3}] [expr {$y+3}]]
    $canvas itemconfigure $l1 {*}$args
    $canvas itemconfigure $l2 {*}$args
}
proc triangleup {canvas x y args} {
    set id [$canvas create polygon $x [expr {$y-4}] \
                [expr {$x+4}] [expr {$y+4}] \
                [expr {$x-4}] [expr {$y+4}]]
    $canvas itemconfigure $id {*}$args
}
proc triangledown {canvas x y args} {
    set id [$canvas create polygon $x [expr {$y+4}] \
                [expr {$x+4}] [expr {$y-4}] \
                [expr {$x-4}] [expr {$y-4}]]
    $canvas itemconfigure $id {*}$args
}

wm withdraw .

#####################################################################
# list creation procedures
proc ones n {
    lrepeat $n 1
}
proc reversed n {
    while {[incr n -1] >= 0} {
        lappend result $n
    }
    return $result
}
proc random n {
    for {set i 0} {$i < $n} {incr i} {
        lappend result [expr {int($n * rand())}]
    }
    return $result
}

set algorithms {lsort quicksort shellsort insertionsort bubblesort mergesort}
set sizes {1 10 100 1000 10000 100000}
set types {ones reversed random}
set shapes {dot square cross triangleup triangledown x}
set colours {red blue black brown yellow black}

# create some lists to be used by all sorting algorithms
array set lists {}
foreach size $sizes {
    foreach type $types {
        set lists($type,$size) [$type $size]
    }
}

set runs 10

# header
fconfigure stdout -buffering none
puts -nonewline [format "%-16s" "list length:"]
foreach size $sizes {
    puts -nonewline [format " %10d" $size]
}
puts ""

# perform the sort timings and output results
foreach type $types {
    puts "\nlist type: $type"
    set times [list]
    foreach algo $algorithms {
        set errs [list]
        set thesetimes [list]
        $algo {} ;# call it once to ensure it's compiled

        puts -nonewline [format "   %-13s" $algo]
        foreach size $sizes {
            # some implementations are just too slow
            if {$type ne "ones" && (
                ($algo eq "insertionsort" && $size > 10000) ||
                ($algo eq "bubblesort" && $size > 1000))
            } {
                set time Inf
            } else {
                # OK, do it
                if {[catch {time [list $algo $lists($type,$size)] $runs} result] != 0} {
                    set time Inf
                    lappend errs $result
                } else {
                    set time [lindex [split $result] 0]
                }
            }
            lappend thesetimes $time
            puts -nonewline [format " %10s" $time]
        }
        puts ""
        if {[llength $errs] > 0} {
            puts [format "      %s" [join $errs "\n      "]]
        }
        lappend times $thesetimes
    }
    create_log10_plot "Sorting a '$type' list" size time $sizes $times $algorithms $shapes $colours
}
puts "\ntimes in microseconds, average of $runs runs"
