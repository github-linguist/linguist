|c|
"Create an ordered collection that will grow while we add elements"
c := OrderedCollection new.
"fill the collection with 9 arrays of 10 elements; elements (objects)
 are initialized to the nil object, which is a well-defined 'state'"
1 to: 9 do: [ :i | c add: (Array new: 10) ].
"However, let us show a way of filling the arrays with object number 0"
c := OrderedCollection new.
1 to: 9 do: [ :i | c add: ((Array new: 10) copyReplacing: nil withObject: 0) ].
"demonstrate that the arrays are distinct: modify the fourth of each"
1 to: 9 do: [ :i | (c at: i) at: 4 put: i ].
"show it"
c do: [ :e | e printNl ].
