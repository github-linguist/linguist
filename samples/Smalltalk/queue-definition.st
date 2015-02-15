OrderedCollection extend [
   push: obj [ ^(self add: obj) ]
   pop [
       (self isEmpty) ifTrue: [
          SystemExceptions.NotFound signalOn: self
                reason: 'queue empty'
       ] ifFalse: [
          ^(self removeFirst)
       ]
   ]
]

|f|
f := OrderedCollection new.
f push: 'example'; push: 'another'; push: 'last'.
f pop printNl.
f pop printNl.
f pop printNl.
f isEmpty printNl.
f pop. "queue empty error"
