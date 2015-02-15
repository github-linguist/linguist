set PI 3.1415927
fconfigure stdout -buffering none
puts -nonewline "Enter latitude       => "; gets stdin lat
puts -nonewline "Enter longitude      => "; gets stdin lng
puts -nonewline "Enter legal meridian => "; gets stdin ref
puts ""

set slat [expr {sin($lat*$PI/180)}]
puts [format "    sine of latitude:   %8g" $slat]
puts [format "    diff longitude:     %3.3f" [expr {$lng - $ref}]]
puts ""
puts "Hour, sun hour angle, dial hour line angle from 6am to 6pm"

for {set h -6} {$h<=6} {incr h} {
    set hra [expr {15.0 * $h}];      # hour angle is 15 times the hour #
    set hra [expr {$hra-$lng+$ref}]; # but correct for longitude difference #
    set hla [expr {atan($slat * tan($hra*$PI/180)) * 180/$PI}]
    puts [format "HR=%+3d; HRA=%+8.3f; HLA=%+8.3f" $h $hra $hla]
}
