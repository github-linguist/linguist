package require Tcl 8.5

puts "enter X dimension:"
set dim2 [gets stdin]
puts "enter Y dimension:"
set dim1 [gets stdin]
# Make the "array"; we'll keep it in row-major form
set l [lrepeat $dim1 [lrepeat $dim2 {}]]
# Select a point at around the middle of the "array"
set y [expr {$dim1>>1}]
set x [expr {$dim2>>1}]
# Set the value at that point
lset l $y $x aValue
# Read the value at that point
puts [lindex $l $y $x]
# Delete the "array"
unset l
