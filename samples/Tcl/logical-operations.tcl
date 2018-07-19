proc logic {a b} {
    puts "a and b: [expr {$a && $b}]"
    puts "a or b:  [expr {$a || $b}]"
    puts "not a:   [expr {!$a}]"
}
