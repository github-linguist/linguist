package require Tcl 8.6

oo::class create Example {
    variable name
    constructor n {set name $n}
    method print {} {puts "Hello, I am $name"}
}
set e [Example new "Eric"]
$e print
set [info object namespace $e]::name "Edith"
$e print
