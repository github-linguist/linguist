!Object subclass: #Point
  instanceVariableNames: 'x y'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'polymorphism' !

!Point class methodsFor: 'instance creation'!
new
  ^self newBasic x := 0; y := 0 ! !

!Point class methodsFor: 'instance creation'!
x: x y: y
  ^self newBasic x := x; y := y ! !

!Point methodsFor: 'member access'!
x
  ^x ! !

!Point methodsFor: 'member access'!
y
  ^y ! !

!Point methodsFor: 'member access'!
x: x
  ^self x := x ! !

!Point methodsFor: 'member access'!
y: y
  ^self y := y ! !

!Point methodsFor: 'member access'!
x: x y: y
  ^self x := x; y := y ! !

!Point methodsFor: 'polymorphism test'!
print
  Transcript show: x; space; show: y ! !

!Object subclass: #Circle
  instanceVariableNames: 'center r'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'polymorphism' !

!Circle class methodsFor: 'instance creation'!
new
  ^self newBasic center := Point new; r := 0 ! !

!Circle class methodsFor: 'instance creation'!
radius: radius
  ^self newBasic center := Point new; r := radius ! !

!Circle class methodsFor: 'instance creation'!
at: point radius: r
  ^self newBasic center := point; r := r ! !

!Circle methodsFor: 'member access'!
center
  ^center ! !

!Circle methodsFor: 'member access'!
x: x y: y
  ^self center x: x y: y ! !

!Circle methodsFor: 'member access'!
radius
  ^r ! !

!Circle methodsFor: 'member access'!
radius: radius
  ^self r := radius ! !

!Circle methodsFor: 'polymorphism test'!
print
  Transcript show: center; space; show: radius ! !
