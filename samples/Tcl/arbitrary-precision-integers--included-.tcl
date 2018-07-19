set bigValue [expr {5**4**3**2}]
puts "5**4**3**2 has [string length $bigValue] digits"
if {[string match "62060698786608744707*92256259918212890625" $bigValue]} {
    puts "Value starts with 62060698786608744707, ends with 92256259918212890625"
} else {
    puts "Value does not match 62060698786608744707...92256259918212890625"
}
