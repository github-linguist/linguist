package require Tcl 8.5

set map [dict create]
set sum 0.0

foreach name {aleph beth gimel daleth he waw zayin} \
        prob {1/5.0 1/6.0 1/7.0 1/8.0 1/9.0 1/10.0 1/11.0} \
{
    set prob [expr $prob]
    set sum [expr {$sum + $prob}]
    dict set map $name [dict create probability $prob limit $sum count 0]
}
dict set map heth [dict create probability [expr {1.0 - $sum}] limit 1.0 count 0]

set samples 1000000
for {set i 0} {$i < $samples} {incr i} {
    set n [expr {rand()}]
    foreach name [dict keys $map] {
        if {$n <= [dict get $map $name limit]} {
            set count [dict get $map $name count]
            dict set map $name count [incr count]
            break
        }
    }
}

puts "using $samples samples:"
puts [format "%-10s %-21s %-9s %s" "" expected actual difference]

dict for {name submap} $map {
    dict with submap {
        set actual [expr {$count * 1.0 / $samples}]
        puts [format "%-10s %-21s %-9s %4.2f%%" $name $probability $actual \
                [expr {abs($actual - $probability)/$probability*100.0}]
             ]
    }
}
