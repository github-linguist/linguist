namespace eval ::myIntType {
    variable value_cache
    array set value_cache {}
    variable type integer
    variable min 1
    variable max 10
    variable errMsg "cannot set %s to %s: must be a $type between $min and $max"
}
proc ::myIntType::declare varname {
    set ns [namespace current]
    uplevel [list trace add variable $varname write ${ns}::write]
    uplevel [list trace add variable $varname unset ${ns}::unset_var]
}
proc ::myIntType::unset_var {varname args} {
    variable value_cache
    unset value_cache($varname)
}
proc ::myIntType::validate {value} {
    variable type
    variable min
    variable max
    expr {[string is $type -strict $value] && $min <= $value && $value <= $max}
}
proc ::myIntType::write {varname args} {
    variable value_cache
    upvar $varname var
    set value $var
    if {[validate $value]} {
        set value_cache($varname) $value
    } else {
        if {[info exists value_cache($varname)]} {
            set var $value_cache($varname)
        }
        variable errMsg
        error [format $errMsg $varname $value]
    }
}
