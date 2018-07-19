namespace import tcl::mathop::*

proc ast str {
    # produce abstract syntax tree for an expression
    regsub -all {[-+*/()]} $str { & } str ;# "tokenizer"
    s $str
}
proc s {args} {
    # parse "(a + b) * c + d" to "+ [* [+ a b] c] d"
    if {[llength $args] == 1} {set args [lindex $args 0]}
    if [regexp {[()]} $args] {
        eval s [string map {( "\[s " ) \]} $args]
    } elseif {"*" in $args} {
	s [s_group $args *]
    } elseif {"/" in $args} {
	s [s_group $args /]
    } elseif {"+" in $args} {
        s [s_group $args +]
    } elseif {"-" in $args} {
        s [s_group $args -]
    } else {
        string map {\{ \[ \} \]} [join $args]
    }
}
proc s_group {list op} {
    # turn ".. a op b .." to ".. {op a b} .."
    set pos [lsearch -exact $list $op]
    set p_1 [- $pos 1]
    set p1  [+ $pos 1]
    lreplace $list $p_1 $p1 \
                  [list $op [lindex $list $p_1] [lindex $list $p1]]
}
#-- Test suite
foreach test [split {
    ast 2-2
    ast 1-2-3
    ast (1-2)-3
    ast 1-(2-3)
    ast (1+2)*3
    ast (1+2)/3-4*5
    ast ((1+2)/3-4)*5
} \n] {
    puts "$test ..... [eval $test] ..... [eval [eval $test]]"
}
