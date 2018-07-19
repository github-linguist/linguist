proc stripchars {str chars} {
    foreach c [split $chars ""] {set str [string map [list $c ""] $str]}
    return $str
}

set s "She was a soul stripper. She took my heart!"
puts [stripchars $s "aei"]
