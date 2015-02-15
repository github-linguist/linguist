proc countSubstrings {haystack needle} {
    regexp -all ***=$needle $haystack
}
puts [countSubstrings "the three truths" "th"]
puts [countSubstrings "ababababab" "abab"]
puts [countSubstrings "abaabba*bbaba*bbab" "a*b"]
