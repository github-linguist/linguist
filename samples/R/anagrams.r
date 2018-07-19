words <- readLines("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
word_group <- sapply(
    strsplit(words, split=""), # this will split all words to single letters...
    function(x) paste(sort(x), collapse="") # ...which we sort and paste again
)

counts <- tapply(words, word_group, length) # group words by class to get number of anagrams
anagrams <- tapply(words, word_group, paste, collapse=", ") # group to get string with all anagrams

# Results
table(counts)
counts
    1     2     3     4     5
22263  1111   155    31     6

anagrams[counts == max(counts)]
                               abel                               acert
     "abel, able, bale, bela, elba" "caret, carte, cater, crate, trace"
                              aegln                               aeglr
"angel, angle, galen, glean, lange" "alger, glare, lager, large, regal"
                               aeln                                eilv
     "elan, lane, lean, lena, neal"      "evil, levi, live, veil, vile"
