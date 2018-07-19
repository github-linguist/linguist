USING: io random math math.parser kernel formatting ;
IN: guess-the-number

<PRIVATE

: gen-number ( -- n )
  10 random 1 + ;

: make-guess ( n -- n ? )
  dup readln string>number = ;

: play-game ( n -- n )
  [ make-guess ]
  [ "Guess a number between 1 and 10:" print flush ] do until ;

PRIVATE>

: guess-the-number ( -- )
  gen-number play-game
  "Yes, the number was %d!\n" printf ;
