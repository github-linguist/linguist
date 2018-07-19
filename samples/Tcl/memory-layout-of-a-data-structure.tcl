set rs232_bits {CD RD TD DTR SG DSR RTS CTS RI}

proc rs232_encode args {
    set res 0
    foreach arg $args {
        set pos [lsearch $::rs232_bits $arg]
        if {$pos >=0} {set res [expr {$res | 1<<$pos}]}
    }
    return $res
}
proc rs232_decode int {
    set res {}
    set i -1
    foreach bit $::rs232_bits {
        incr i
        if {$int & 1<<$i} {lappend res $bit}
    }
    return $res
}
#------------------------------ Test suite
foreach {test => expected} {
    {rs232_encode CD} -> 1
    {rs232_decode 1} -> CD
    {rs232_encode CD RD TD} -> 7
    {rs232_decode 7} -> {CD RD TD}
} {
    catch $test res
    if {$res ne $expected} {puts "$test -> $res, expected $expected"}
}
