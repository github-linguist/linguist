package require Tcl 8.5

proc stoogesort {L {i 0} {j -42}} {
   if {$j == -42} {# Magic marker
      set j [expr {[llength $L]-1}]
   }
   set Li [lindex $L $i]
   set Lj [lindex $L $j]
   if {$Lj < $Li} {
      lset L $i $Lj
      lset L $j $Li
   }
   if {$j-$i > 1} {
      set t [expr {($j-$i+1)/3}]
      set L [stoogesort $L $i [expr {$j-$t}]]
      set L [stoogesort $L [expr {$i+$t}] $j]
      set L [stoogesort $L $i [expr {$j-$t}]]
   }
   return $L
}

stoogesort {1 4 5 3 -6 3 7 10 -2 -5}
