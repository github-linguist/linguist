package require critcl
critcl::code {
    #include <math.h>
}
critcl::cproc tcl::mathfunc::ilogb {double value} int {
    return ilogb(value);
}
package provide ilogb 1.0
