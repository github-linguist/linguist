package require Tcl 8.5
package require http

set url http://www.puzzlers.org/pub/wordlists/unixdict.txt
set response [http::geturl $url]
set data [http::data $response]
http::cleanup $response

set max 0
array set anagrams {}

foreach line [split $data \n] {
    foreach word [split $line] {
        set anagram [join [lsort [split $word ""]] ""]
        lappend anagrams($anagram) $word
        set max [::tcl::mathfunc::max $max [llength $anagrams($anagram)]]
    }
}

foreach key [array names anagrams] {
    if {[llength $anagrams($key)] == $max} {
        puts $anagrams($key)
    }
}
