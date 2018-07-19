Object subclass: Hammer [
  Hammer class >> hammingNumbers: howMany [
    |h i j k x2 x3 x5|
      h := OrderedCollection new.
      i := 0. j := 0. k := 0.
      h add: 1.
      x2 := 2. x3 := 2. x5 := 5.
      [ ( h size) < howMany ] whileTrue: [
        |m|
        m := { x2. x3. x5 } sort first.
        (( h indexOf: m ) = 0) ifTrue: [ h add: m ].
        ( x2 = (h last) ) ifTrue: [ i := i + 1. x2 := 2 * (h at: i) ].
        ( x3 = (h last) ) ifTrue: [ j := j + 1. x3 := 3 * (h at: j) ].
        ( x5 = (h last) ) ifTrue: [ k := k + 1. x5 := 5 * (h at: k) ].
      ].
      ^ h sort
  ]
].

(Hammer hammingNumbers: 20) displayNl.
(Hammer hammingNumbers: 1690) last displayNl.
