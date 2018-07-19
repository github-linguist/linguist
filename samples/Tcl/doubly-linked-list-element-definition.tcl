oo::class create List {
    variable content next prev
    constructor {value {list ""}} {
        set content $value
        set next $list
        set prev ""
        if {$next ne ""} {
            $next previous [self]
        }
    }
    method value args {
        set content {*}$args
    }
    method next args {
        set next {*}$args
    }
    method previous args {
        set prev {*}$args
    }
}
