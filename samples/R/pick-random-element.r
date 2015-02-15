# a vector (letters are builtin)
letters
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"

# picking one element
sample(letters, 1)
# [1] "n"

# picking some elements with repetition, and concatenating to get a word
paste(sample(letters, 10, rep=T), collapse="")
# [1] "episxgcgmt"
