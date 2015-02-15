set x {1 2 3 1e11}
foreach a $x {lappend y [expr {sqrt($a)}]}
set fh [open sqrt.dat w]
foreach a $x b $y {
    puts $fh [format "%.*g\t%.*g" $xprecision $a $yprecision $b]
}
close $fh

set fh [open sqrt.dat]
puts [read $fh [file size sqrt.dat]]
close $fh
