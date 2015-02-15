# string creation
set x "hello world"

# string destruction
unset x

# string assignment with a null byte
set x a\0b
string length $x ;# ==> 3

# string comparison
if {$x eq "hello"} {puts equal} else {puts "not equal"}
set y bc
if {$x < $y} {puts "$x is lexicographically less than $y"}

# string copying; cloning happens automatically behind the scenes
set xx $x

# check if empty
if {$x eq ""} {puts "is empty"}
if {[string length $x] == 0} {puts "is empty"}

# append a byte
append x \07

# substring
set xx [string range $x 0 end-1]

# replace bytes
set y [string map {l L} "hello world"]

# join strings
set a "hel"
set b "lo w"
set c "orld"
set d $a$b$c
