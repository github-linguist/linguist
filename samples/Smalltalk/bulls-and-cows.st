Object subclass: BullsCows [
  |number|
  BullsCows class >> new: secretNum [ |i|
    i := self basicNew.
    (self isValid: secretNum)
       ifFalse: [ SystemExceptions.InvalidArgument
                    signalOn: secretNum
                    reason: 'You need 4 unique digits from 1 to 9' ].
    i setNumber: secretNum.
    ^ i
  ]
  BullsCows class >> new [ |b| b := Set new.
     [ b size < 4 ]
       whileTrue: [ b add: ((Random between: 1 and: 9) displayString first) ].
     ^ self new: (b asString)
  ]
  BullsCows class >> isValid: num [
    ^ (num asSet size = 4) & ((num asSet includes: $0) not)
  ]
  setNumber: num [ number := num ]
  check: guess [ |bc| bc := Bag new.
     1 to: 4 do: [ :i |
       (number at: i) = (guess at: i)
         ifTrue: [ bc add: 'bulls' ]
         ifFalse: [
             (number includes: (guess at: i))
               ifTrue: [ bc add: 'cows' ]
         ]
     ].
     ^ bc
  ]
].

'Guess the 4-digits number (digits from 1 to 9, no repetition)' displayNl.

|guessMe d r tries|
[
  tries := 0.
  guessMe := BullsCows new.
  [
    [
      'Write 4 digits: ' display.
      d := stdin nextLine.
      (BullsCows isValid: d)
    ] whileFalse: [
         'Insert 4 digits, no repetition, exclude the digit 0' displayNl
    ].
    r := guessMe check: d.
    tries := tries + 1.
    (r occurrencesOf: 'bulls') = 4
  ] whileFalse: [
    ('%1 cows, %2 bulls' % { r occurrencesOf: 'cows'. r occurrencesOf: 'bulls' })
      displayNl.
  ].
  ('Good, you guessed it in %1 tries!' % { tries }) displayNl.
  'Do you want to play again? [y/n]' display.
  ( (stdin nextLine) = 'y' )
] whileTrue: [ Character nl displayNl ].
