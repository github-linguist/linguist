package require Tcl 8.5

# Bitwise test is the most efficient
proc tcl::mathfunc::isOdd x  { expr {$x & 1} }
proc tcl::mathfunc::isEven x { expr {!($x & 1)} }

puts " # O E"
puts 24:[expr isOdd(24)],[expr isEven(24)]
puts 49:[expr isOdd(49)],[expr isEven(49)]
