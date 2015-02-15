namespace eval LZW {
    variable char2int
    variable chars
    for {set i 0} {$i < 256} {incr i} {
        set char [binary format c $i]
        set char2int($char) $i
        lappend chars $char
    }
}

proc LZW::encode {data} {
    variable char2int
    array set dict [array get char2int]

    set w ""
    set result [list]

    foreach c [split $data ""] {
        set wc $w$c
        if {[info exists dict($wc)]} {
            set w $wc
        } else {
            lappend result $dict($w)
            set dict($wc) [array size dict]
            set w $c
        }
    }
    lappend result $dict($w)
}

proc LZW::decode {cdata} {
    variable chars
    set dict $chars

    set k [lindex $cdata 0]
    set w [lindex $dict $k]
    set result $w

    foreach k [lrange $cdata 1 end] {
        set currSizeDict [llength $dict]
        if {$k < $currSizeDict} {
            set entry [lindex $dict $k]
        } elseif {$k == $currSizeDict} {
            set entry $w[string index $w 0]
        } else {
            error "invalid code ($k) in ($cdata)"
        }
        append result $entry
        lappend dict $w[string index $entry 0]
        set w $entry
    }
    return $result
}

set s TOBEORNOTTOBEORTOBEORNOT#
set e [LZW::encode $s] ;# ==> 84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263 35
set d [LZW::decode $e] ;# ==> TOBEORNOTTOBEORTOBEORNOT#

# or
if {$s eq [LZW::decode [LZW::encode $s]]} then {puts success} else {puts fail} ;# ==> success
