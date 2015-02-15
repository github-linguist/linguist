Object subclass: #Card
  instanceVariableNames: 'thePip theSuit'
  classVariableNames: 'pips suits'
  poolDictionaries: ''
  category: nil !

!Card class methods!
initialize
  suits ifNil: [ suits := 'clubs,hearts,spades,diamonds' subStrings: $, ].
  pips ifNil: [ pips := '2,3,4,5,6,7,8,9,10,jack,queen,king,ace' subStrings: $, ]
!
new
  | o |
  o := super new.
  ^o
!
new: card
  | o |
  o := self new.
  o initWithPip: (card at: 1) andSuit: (card at: 2).
  ^o
!!

!Card class methods !
pips
  Card initialize.
  ^pips
!
suits
  Card initialize.
  ^suits
!!

!Card methods!
initWithPip: aPip andSuit: aSuit
  ( (pips includes: aPip asLowercase) &
    (suits includes: aSuit asLowercase) )
     ifTrue: [
          thePip := aPip copy.
          theSuit := aSuit copy
     ] ifFalse: [ 'Unknown pip or suit' displayOn: stderr .
                  Character nl displayOn: stderr ].
  ^self
!
asString
  ^('(%1,%2)' % { thePip . theSuit })
!
display
  self asString display
!
displayNl
  self display.
  Character nl display
!!


Object subclass: #Deck
  instanceVariableNames: 'deck'
  classVariableNames: ''
  poolDictionaries: ''
  category: nil !

!Deck class methods !
new
  |d|
  d := super new.
  d init.
  ^d
!!

!Deck methods !
init
   deck := OrderedCollection new.
   Card suits do: [ :suit |
     Card pips do: [ :pip |
         deck add: (Card new: { pip . suit })
     ]
   ]
!
deck
  ^deck
!
shuffle
  1 to: self deck size do: [ :i |
     |r2 o|
     r2 := Random between: 1 and: self deck size.
     o := self deck at: i.
     self deck at: i put: (self deck at: r2).
     self deck at: r2 put: o
  ].
  ^self
!
display
  self deck do: [ :card |
     card displayNl
  ]
!
deal
  ^self deck removeFirst
!!

"create a deck, shuffle it, remove the first card and display it"
Deck new shuffle deal displayNl.
