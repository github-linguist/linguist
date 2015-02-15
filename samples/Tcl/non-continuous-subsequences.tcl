 proc subsets l {
     set res [list [list]]
     foreach e $l {
         foreach subset $res {lappend res [lappend subset $e]}
     }
     return $res
 }
 proc is_not_continuous seq {
     set last [lindex $seq 0]
     foreach e [lrange $seq 1 end] {
         if {$e-1 != $last} {return 1}
         set last $e
     }
     return 0
 }
 proc lfilter {f list} {
     set res {}
     foreach i $list {if [$f $i] {lappend res $i}}
     return $res
 }

% lfilter is_not_continuous [subsets {1 2 3 4}]
{1 3} {1 4} {2 4} {1 2 4} {1 3 4}
