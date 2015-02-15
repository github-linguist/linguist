package require Tcl 8.5

namespace eval playing_cards {
    variable deck
    #variable suits {C D H S}
    variable suits {\u2663 \u2662 \u2661 \u2660}
    variable pips {2 3 4 5 6 7 8 9 10 J Q K A}

    proc new_deck {} {
        variable deck
        set deck [list]
        for {set i 0} {$i < 52} {incr i} {
            lappend deck $i
        }
    }

    proc shuffle {} {
        variable deck
        # shuffle in place
        for {set i 51} {$i > 0} {incr i -1} {
            set n [expr {int($i * rand())}]
            set card [lindex $deck $n]
            lset deck $n [lindex $deck $i]
            lset deck $i $card
        }
    }

    proc deal {{num 1}} {
        variable deck
        incr num -1
        set cards [lrange $deck 0 $num]
        set deck [lreplace $deck 0 $num]
        return $cards
    }

    proc card2string {card} {
        variable suits
        variable pips
        set suit [expr {$card / 13}]
        set pip [expr {$card % 13}]
        return [format "%2s %s" [lindex $pips $pip] [lindex $suits $suit]]
    }

    proc print {cards args} {
        array set opts [concat -sort false $args]
        if {$opts(-sort)} {
            set cards [lsort -integer $cards]
        }
        foreach card $cards {
            puts [card2string $card]
        }
    }

    proc print_deck {} {
        variable deck
        print $deck
    }
}

playing_cards::new_deck
playing_cards::shuffle
set hand [playing_cards::deal 5]
puts "my hand:"
playing_cards::print $hand -sort true
puts "\nthe deck:"
playing_cards::print_deck
