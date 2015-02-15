# assume $xml holds the XML data
package require tdom
set doc [dom parse $xml]
set root [$doc documentElement]

set allNames [$root selectNodes //name]
puts [llength $allNames] ;# ==> 4

set firstItem [lindex [$root selectNodes //item] 0]
puts [$firstItem @upc] ;# ==> 123456789

foreach node [$root selectNodes //price] {
    puts [$node text]
}
