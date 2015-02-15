Smalltalk at: #a put: nil.
Smalltalk at: #b put: nil.

a := [:x| 'executing a' displayNl. x].
b := [:x| 'executing b' displayNl. x].

('false and false = %1' %
  { (a value: false) and: [ b value: false ] })
    displayNl.

('true or false = %1' %
  { (a value: true) or: [ b value: false ] })
    displayNl.

('false or true = %1' %
  { (a value: false) or: [ b value: true ] })
    displayNl.

('true and false = %1' %
  { (a value: true) and: [ b value: false ] })
    displayNl.
