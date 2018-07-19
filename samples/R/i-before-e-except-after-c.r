words = tolower(readLines("http://www.puzzlers.org/pub/wordlists/unixdict.txt"))
ie.npc = sum(grepl("(?<!c)ie", words, perl = T))
ei.npc = sum(grepl("(?<!c)ei", words, perl = T))
ie.pc = sum(grepl("cie", words, fixed = T))
ei.pc = sum(grepl("cei", words, fixed = T))

p1 = ie.npc > 2 * ei.npc
p2 = ei.pc > 2 * ie.pc

message("(1) is ", (if (p1) "" else "not "), "plausible.")
message("(2) is ", (if (p2) "" else "not "), "plausible.")
message("The whole phrase is ", (if (p1 && p2) "" else "not "), "plausible.")
