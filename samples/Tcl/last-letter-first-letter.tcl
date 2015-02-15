proc search {path arcs} {
    set solutions {}
    set c [string index [lindex $path end] end]
    set i -1
    foreach arc $arcs {
	incr i
	if {[string index $arc 0] ne $c} continue
	set soln [search [concat $path [list $arc]] [lreplace $arcs $i $i]]
	lappend solutions [list [llength $soln] $soln]
    }
    if {[llength $solutions]} {
	return [lindex [lsort -integer -decreasing -index 0 $solutions] 0 1]
    } else {
	return $path
    }
}
proc firstlast names {
    set solutions {}
    set i -1
    foreach initial $names {
	incr i
	set soln [search [list $initial] [lreplace $names $i $i]]
	lappend solutions [list [llength $soln] $soln]
    }
    return [lindex [lsort -integer -decreasing -index 0 $solutions] 0 1]
}

set names {
    audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
    cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
    girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff
    kangaskhan kricketune landorus ledyba loudred lumineon lunatone machamp
    magnezone mamoswine nosepass petilil pidgeotto pikachu pinsir poliwrath
    poochyena porygon2 porygonz registeel relicanth remoraid rufflet sableye
    scolipede scrafty seaking sealeo silcoon simisear snivy snorlax spoink
    starly tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle
    whismur wingull yamask
}
set path [firstlast $names]
puts "Path (length: [llength $path]): $path"
