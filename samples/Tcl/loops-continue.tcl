for {set i 1} {$i <= 10} {incr i} {
   puts -nonewline $i
   if {$i % 5 == 0} {
      puts ""
      continue
   }
   puts -nonewline ", "
}
