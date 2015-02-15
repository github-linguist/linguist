package require Tcl 8.6
oo::class create Example {
    method foo {} {return 42}
    method 1 {s} {puts "fee$s"}
    method 2 {s} {puts "fie$s"}
    method 3 {s} {puts "foe$s"}
    method 4 {s} {puts "fum$s"}
}
set eg [Example new]
set mthd [format "%c%c%c" 102 111 111];    # A "foo" by any other means would smell as sweet
puts [$eg $mthd]
for {set i 1} {$i <= 4} {incr i} {
    $eg $i ...
}
