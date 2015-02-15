# Check if we're using one of the UTF or "unicode" encodings
if {[string match utf-* [encoding system]] || [string match *unicode* [encoding system]]} {
    puts "\u25b3"
} else {
    error "terminal does not support unicode (probably)"
}
