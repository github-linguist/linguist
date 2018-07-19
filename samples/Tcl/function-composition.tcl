package require Tcl 8.5
namespace path {::tcl::mathfunc}

proc compose {f g} {
    list apply [list {f g x} {{*}$f [{*}$g $x]}] $f $g]
}

set sin_asin [compose sin asin]
{*}$sin_asin 0.5 ;# ==> 0.5
{*}[compose abs int] -3.14 ;# ==> 3
