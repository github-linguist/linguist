Object subclass: #MyClass
  instanceVariableNames: 'instanceVar'
  classVariableNames: 'classVar'
  poolDictionaries: ''
  category: 'Testing' !

!MyClass class methodsFor: 'instance creation'!
new
  ^self basicNew  instanceVar := 0 ! !

!MyClass methodsFor: 'testing'!
someMethod
  ^self instanceVar = 1; classVar = 3 ! !

MyClass new someMethod!
