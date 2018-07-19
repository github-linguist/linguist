package require math::complexnumbers
namespace import math::complexnumbers::complex math::complexnumbers::tostring

proc quadratic {a b c} {
    set discrim [expr {$b**2 - 4*$a*$c}]
    set roots [list]
    if {$discrim < 0} {
        set term1 [expr {(-1.0*$b)/(2*$a)}]
        set term2 [expr {sqrt(abs($discrim))/(2*$a)}]
        lappend roots [tostring [complex $term1 $term2]] \
                [tostring [complex $term1 [expr {-1 * $term2}]]]
    } elseif {$discrim == 0} {
        lappend roots [expr {-1.0*$b / (2*$a)}]
    } else {
        lappend roots [expr {(-1*$b + sqrt($discrim)) / (2 * $a)}] \
                [expr {(-1*$b - sqrt($discrim)) / (2 * $a)}]
    }
    return $roots
}

proc report_quad {a b c} {
    puts [format "%sx**2 + %sx + %s = 0" $a $b $c]
    foreach root [quadratic $a $b $c] {
        puts "    x = $root"
    }
}

# examples on this page
report_quad 3 4 [expr {4/3.0}] ;# {-2/3}
report_quad 3 2 -1    ;# {1/3, -1}
report_quad 3 2  1    ;# {(-1/3 + sqrt(2/9)i), (-1/3 - sqrt(2/9)i)}
report_quad 1 0  1    ;# {(0+i), (0-i)}
report_quad 1 -1e6 1  ;# {1e6, 1e-6}
# examples from http://en.wikipedia.org/wiki/Quadratic_equation
report_quad -2  7 15  ;# {5, -3/2}
report_quad  1 -2  1  ;# {1}
report_quad  1  3  3  ;# {(-3 - sqrt(3)i)/2), (-3 + sqrt(3)i)/2)}
