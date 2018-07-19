|re s s1|
re := Regex fromString: '[a-z]+ing'.
s := 'this is a matching string'.
s1 := 'this does not match'.

(s =~ re)
ifMatched: [ :b |
   b match displayNl
].
(s1 =~ re)
ifMatched: [ :b |
   'Strangely matched!' displayNl
]
ifNotMatched: [
   'no match!' displayNl
].

(s replacingRegex: re with: 'modified') displayNl.
