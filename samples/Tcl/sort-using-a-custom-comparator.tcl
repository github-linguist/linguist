proc sorter {a b} {
    set la [string length $a]
    set lb [string length $b]
    if {$la < $lb} {
        return 1
    } elseif {$la > $lb} {
        return -1
    }
    return [string compare [string tolower $a] [string tolower $b]]
}

set strings {here are Some sample strings to be sorted}
lsort -command sorter $strings ;# ==> strings sample sorted here Some are be to
