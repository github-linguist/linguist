oo::class create List {
    variable content next
    constructor {value {list ""}} {
        set content $value
        set next $list
    }
    method value args {
        set content {*}$args
    }
    method attach {list} {
        set next $list
    }
    method detach {} {
        set next ""
    }
    method next {} {
        return $next
    }
    method print {} {
        for {set n [self]} {$n ne ""} {set n [$n next]} {
            lappend values [$n value]
        }
        return $values
    }
}
