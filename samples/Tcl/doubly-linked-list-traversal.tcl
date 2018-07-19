# Modify the List class to add the iterator methods
oo::define List {
    method foreach {varName script} {
        upvar 1 $varName v
        for {set node [self]} {$node ne ""} {set node [$node next]} {
            set v [$node value]
            uplevel 1 $script
        }
    }
    method revforeach {varName script} {
        upvar 1 $varName v
        for {set node [self]} {$node ne ""} {set node [$node previous]} {
            set v [$node value]
            uplevel 1 $script
        }
    }
}

# Demonstrating...
set first [List new a [List new b [List new c [set last [List new d]]]]]
puts "Forward..."
$first foreach char { puts $char }
puts "Backward..."
$last revforeach char { puts $char }
