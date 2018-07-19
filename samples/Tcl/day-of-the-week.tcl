package require Tcl 8.5

for {set y 2008} {$y <= 2121} {incr y} {
    if {[clock format [clock scan "$y-12-25" -format {%Y-%m-%d}] -format %w] == 0} {
        puts "xmas $y is a sunday"
    }
}
