proc numlist< {A B} {
    foreach a $A b $B {
        if {$a<$b} {
            return 1
        } elseif {$a>$b} {
            return 0
        }
    }
    return 0
}
