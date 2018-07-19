package require Tcl 8.5
proc haversineFormula {lat1 lon1 lat2 lon2} {
    set rads [expr atan2(0,-1)/180]
    set R 6372.8    ;# In kilometers

    set dLat [expr {($lat2-$lat1) * $rads}]
    set dLon [expr {($lon2-$lon1) * $rads}]
    set lat1 [expr {$lat1 * $rads}]
    set lat2 [expr {$lat2 * $rads}]

    set a [expr {sin($dLat/2)**2 + sin($dLon/2)**2*cos($lat1)*cos($lat2)}]
    set c [expr {2*asin(sqrt($a))}]
    return [expr {$R * $c}]
}

# Don't bother with too much inappropriate accuracy!
puts [format "distance=%.1f km" [haversineFormula 36.12 -86.67 33.94 -118.40]]
