#!/usr/bin/env tclsh
proc main argv {
    array set value  {panacea 3000  ichor 1800  gold 2500}
    array set weight {panacea 0.3   ichor 0.2   gold 2.0   max 25}
    array set volume {panacea 0.025 ichor 0.015 gold 0.002 max 0.25}

    foreach i {panacea ichor gold} {
        set max($i) [expr {min(int($volume(max)/$volume($i)),
                               int($weight(max)/$weight($i)))}]
    }
    set maxval 0
    for {set i 0} {$i < $max(ichor)} {incr i} {
        for {set p 0} {$p < $max(panacea)} {incr p} {
            for {set g 0} {$g < $max(gold)} {incr g} {
                if {$i*$weight(ichor) + $p*$weight(panacea) + $g*$weight(gold)
                    > $weight(max)} continue
                if {$i*$volume(ichor) + $p*$volume(panacea) + $g*$volume(gold)
                    > $volume(max)} continue
                set val [expr {$i*$value(ichor)+$p*$value(panacea)+$g*$value(gold)}]
                if {$val == $maxval} {
                    lappend best [list i $i p $p g $g]
                } elseif {$val > $maxval} {
                    set maxval $val
                    set best [list [list i $i p $p g $g]]
                }
            }
        }
    }
    puts "maxval: $maxval, best: $best"
}
main $argv
