Number extend [
  ** anInt [
       | r |
       ( anInt isInteger )
            ifFalse:
              [ '** works fine only for integer powers'
	        displayOn: stderr . Character nl displayOn: stderr ].
       r := 1.
       1 to: anInt do: [ :i | r := ( r * self ) ].
       ^r
  ]
].

( 2.5 ** 3 ) displayNl.
( 2 ** 10 ) displayNl.
( 3/7 ** 3 ) displayNl.
