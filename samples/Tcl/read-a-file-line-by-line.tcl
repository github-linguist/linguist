set f [open "foobar.txt"]
while {[gets $f line] >= 0} {
    # This loops over every line
    puts ">>$line<<"
}
close $f
