package require soundex

foreach string {"Soundex" "Example" "Sownteks" "Ekzampul"} {
    set soundexCode [soundex::knuth $string]
    puts "\"$string\" has code $soundexCode"
}
