cd /tmp

# Create the file
set f [open hello.jnk w]
puts $f "Hello World!"
close $f

# Archive to tape
set fin [open "|tar cf - hello.jnk" rb]
set fout [open /dev/tape wb]
fcopy $fin $fout
close $fin
close $fout
