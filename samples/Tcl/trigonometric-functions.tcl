package require Tcl 8.5

proc PI {} {expr {4*atan(1)}}
proc deg2rad d {expr {$d/180*[PI]}}
proc rad2deg r {expr {$r*180/[PI]}}

namespace path ::tcl::mathfunc

proc trig degrees {
    set radians [deg2rad $degrees]
    puts [sin $radians]
    puts [cos $radians]
    puts [tan $radians]
    set arcsin [asin [sin $radians]]; puts "$arcsin [rad2deg $arcsin]"
    set arccos [acos [cos $radians]]; puts "$arccos [rad2deg $arccos]"
    set arctan [atan [tan $radians]]; puts "$arctan [rad2deg $arctan]"
}
trig 60.0
