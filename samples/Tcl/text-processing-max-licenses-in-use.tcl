 set out 0
 set max_out -1
 set max_times {}

 foreach job [split [read [open "mlijobs.txt" "r"]] "\n"] {
     if {[lindex $job 1] == "OUT"} {
         incr out
     } {
         incr out -1
     }
     if {$out > $max_out} {
         set max_out $out
         set max_times {}
     }
     if {$out == $max_out} {
         lappend max_times [lindex $job 3]
     }
 }

 puts "Maximum simultaneous license use is $max_out at the following times:"
 foreach t $max_times {
     puts "  $t"
 }
