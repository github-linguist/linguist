package require control

set x 5
control::assert {$x == 42}
