| s v t sum hm |
"uncomment the following to see what happens if bloop exists"
"Smalltalk at: #bloop put: -10."
s := Smalltalk version.
(s =~ '(\d+)\.(\d+)\.(\d+)')
  ifMatched: [:match |
       v := (( (match at: 1) asInteger ) * 100) +
            (( (match at: 2) asInteger ) * 10) +
            ( (match at: 3) asInteger )
  ].
( v < 300 )
  ifTrue: [
     Transcript show: 'I need version 3.0.0 or later' ; cr ]
  ifFalse: [
     Transcript show: 'Ok! I can run!' ; cr .
     "does bloop exists as global var?"
     t := Smalltalk at: #bloop
            ifAbsent: [
                 Transcript show: 'bloop var does not exist as global!' ; cr .
                 ^nil
            ].
     (t respondsTo: #abs)
          ifTrue:
             [ Transcript show: 'Absolute value of bloop: ' ;
                     show: (t abs) printString ; cr ].
  ] .

"how many 'numbers' in global scope, and compute their sums"
hm := 0.
sum := 0.
(Smalltalk keys) do: [ :els |
        ( (Smalltalk at: els) isKindOf: Number )
          ifTrue: [ hm := hm + 1.
                    sum := sum + (Smalltalk at: els).
                    "Transcript show: (els asString) ; cr" ]
    ] .
Transcript show: 'Num of global numeric vars: '; show: (hm printString); cr ;
           show: 'Sum of global numeric vars: '; show: (sum printString) ; cr.
