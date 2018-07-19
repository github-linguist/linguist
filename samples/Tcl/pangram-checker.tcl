proc pangram? {sentence} {
    set letters [regexp -all -inline {[a-z]} [string tolower $sentence]]
    expr {
        [llength [lsort -unique $letters]] == 26
    }
}

puts [pangram? "This is a sentence"];  # ==> false
puts [pangram? "The quick brown fox jumps over the lazy dog."]; # ==> true
