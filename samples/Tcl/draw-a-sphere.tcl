proc grey {n} {format "#%2.2x%2.2x%2.2x" $n $n $n}

pack [canvas .c -height 400 -width 640 -background white]

for {set i 0} {$i < 255} {incr i} {
  set h [grey $i]
  .c create arc [expr {100+$i/5}] [expr {50+$i/5}] [expr {400-$i/1.5}] [expr {350-$i/1.5}] \
                 -start 0 -extent 359 -fill $h -outline $h}
}
