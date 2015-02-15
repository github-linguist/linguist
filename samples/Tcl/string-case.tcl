set string alphaBETA

# three built-in case conversion commands
string toupper $string  ;# ==> ALPHABETA
string tolower $string  ;# ==> alphabeta
string totitle $string  ;# ==> Alphabeta

# not built-in
proc swapcase {s} {
    foreach char [split $s ""] {
        if {$char eq [set CHAR [string toupper $char]]} {
            append new [string tolower $char]
        } else {
            append new $CHAR
        }
    }
    return $new
}
swapcase $string  ;# ==> ALPHAbeta

# better performance, but English alphabet only
proc swapcase_en {s} {
    string map {
        a A b B c C d D e E f F g G h H i I j J k K l L m M n N o O p P q Q r R s S t T u U v V w W x X y Y z Z
        A a B b C c D d E e F f G g H h I i J j K k L l M m N n O o P p Q q R r S s T t U u V v W w X x Y y Z z
    } $s
}

swapcase Père     ;# ==> pÈRE
swapcase_en Père  ;# ==> pèRE
