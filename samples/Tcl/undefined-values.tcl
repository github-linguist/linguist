# Variables are undefined by default and do not need explicit declaration

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at first check"}

# Give it a value
set var "Screwy Squirrel"

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at second check"}

# Remove its value
unset var

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at third check"}

# Give it a value again
set var 12345

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at fourth check"}

puts "Done"
