   #words=: <;._2 ] 1!:1 <'unixdict.txt'
25104
   #anagrams=: (#~ 1 < #@>) (</.~ /:~&>) words
1303
   #maybederanged=: (#~ (1 -.@e. #@~."1)@|:@:>&>) anagrams
432
   #longest=: (#~ [: (= >./) #@>@{.@>) maybederanged
1
   longest
┌───────────────────────┐
│┌──────────┬──────────┐│
││excitation│intoxicate││
│└──────────┴──────────┘│
└───────────────────────┘
