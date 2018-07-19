package require struct::set

set A {John Bob Mary Serena}
set B {Jim Mary John Bob}

set AnotB   [struct::set difference $A $B]
set BnotA   [struct::set difference $B $A]
set SymDiff [struct::set union $AnotB $BnotA]

puts "A\\B = $AnotB"
puts "B\\A = $BnotA"
puts "A\u2296B = $SymDiff"

# Of course, the library already has this operation directly...
puts "Direct Check: [struct::set symdiff $A $B]"
