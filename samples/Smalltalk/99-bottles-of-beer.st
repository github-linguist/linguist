Smalltalk at: #sr put: 0 ; at: #s put: 0 !
sr := Dictionary new.
sr at: 0 put: ' bottle' ;
   at: 1 put: ' bottles' ;
   at: 2 put: ' of beer' ;
   at: 3 put: ' on the wall' ;
   at: 4 put: 'Take one down, pass it around' !
99 to: 0 by: -1 do: [:v | v print.
         ( v == 1 ) ifTrue: [ s := 0. ]
	            ifFalse: [ s := 1. ].
	 Transcript show: (sr at:s) ; show: (sr at:2) ; show: (sr at:3) ; cr.
	            v print.
	 Transcript show: (sr at:s) ; show: (sr at:2) ; cr.
		    (v ~~ 0) ifTrue: [ Transcript show: (sr at:4) ; cr. ].
   ].
