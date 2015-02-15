package require math
package require math::calculus

# gamma(1) and gamma(1.5)

set f 1.0
set f2 [expr {sqrt(acos(-1.))/2.}]

for {set x 1.0} {$x <= 10.0} {set x [expr {$x + 0.5}]} {

    # method 1 - numerical integration, Romberg's method, special
    #            case for an improper integral

    set g1 [math::calculus::romberg  \
                [list apply {{x t} {expr {$t ** ($x-1) * exp(-$t)}}} $x] \
                0 1 -relerror 1e-8]
    set g2 [math::calculus::romberg_infinity \
                [list apply {{x t} {expr {$t ** ($x-1) * exp(-$t)}}} $x] \
                1 Inf -relerror 1e-8]
    set gamma [expr {[lindex $g1 0] + [lindex $g2 0]}]

    # method 2 - library function

    set libgamma [expr {exp([math::ln_Gamma $x])}]

    # method 3 - special forms for integer and half-integer arguments

    if {$x == entier($x)} {
        puts [format {%4.1f %13.6f %13.6f %13.6f} $x $gamma $libgamma $f]
        set f [expr $f * $x]
    } else {
        puts [format {%4.1f %13.6f %13.6f %13.6f} $x $gamma $libgamma $f2]
        set f2 [expr $f2 * $x]
    }
}
