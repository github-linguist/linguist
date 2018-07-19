Object subclass: Evolution [
  |target parent mutateRate c alphabet fitness|

  Evolution class >> newWithRate: rate andTarget: aTarget [
    |r| r := super new.
    ^r initWithRate: rate andTarget: aTarget.
  ]

  initWithRate: rate andTarget: aTarget [
    target := aTarget.
    self mutationRate: rate.
    self maxCount: 100.
    self defaultAlphabet.
    self changeParent.
    self fitness: (self defaultFitness).
    ^self
  ]

  defaultFitness [
    ^ [:p :t |
         |t1 t2 s|
         t1 := p asOrderedCollection.
         t2 := t asOrderedCollection.
         s := 0.
         t2 do: [:e| (e == (t1 removeFirst)) ifTrue: [ s:=s+1 ] ].
         s / (target size)
      ]
  ]

  defaultAlphabet [ alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ' asOrderedCollection. ]

  maxCount: anInteger [ c := anInteger ]

  mutationRate: aFloat [ mutateRate := aFloat ]

  changeParent [
      parent := self generateStringOfLength: (target size) withAlphabet: alphabet.
      ^ parent.
  ]

  generateStringOfLength: len withAlphabet: ab [
    |r|
    r := String new.
    1 to: len do: [ :i |
      r := r , ((ab at: (Random between: 1 and: (ab size))) asString)
    ].
    ^r
  ]

  fitness: aBlock [ fitness := aBlock ]

  randomCollection: d [
    |r| r := OrderedCollection new.
    1 to: d do: [:i|
      r add: (Random next)
    ].
    ^r
  ]

  mutate [
    |r p nmutants s|
    r := parent copy.
    p := self randomCollection: (r size).
    nmutants := (p select: [ :e | (e < mutateRate)]) size.
    (nmutants > 0)
       ifTrue: [ |t|
            s := (self generateStringOfLength: nmutants withAlphabet: alphabet) asOrderedCollection.
            t := 1.
            (p collect: [ :e | e < mutateRate ]) do: [ :v |
               v ifTrue: [ r at: t put: (s removeFirst) ].
               t := t + 1.
            ]
       ].
    ^r
  ]

  evolve [
     |children es mi mv|
     es := self getEvolutionStatus.
     children := OrderedCollection new.
     1 to: c do: [ :i |
       children add: (self mutate)
     ].
     children add: es.
     mi := children size.
     mv := fitness value: es value: target.
     children doWithIndex: [:e :i|
       (fitness value: e value: target) > mv
          ifTrue: [ mi := i. mv := fitness value: e value: target ]
     ].
     parent := children at: mi.
     ^es "returns the parent, not the evolution"
  ]

  printgen: i [
    ('%1 %2 "%3"' % {i . (fitness value: parent value: target) . parent }) displayNl
  ]

  evoluted [ ^ target = parent ]
  getEvolutionStatus [ ^ parent ]

].

|organism j|

organism := Evolution newWithRate: 0.01 andTarget: 'METHINKS IT IS LIKE A WEASEL'.

j := 0.
[ organism evoluted ]
  whileFalse: [
    j := j + 1.
    organism evolve.
    ((j rem: 20) = 0) ifTrue: [ organism printgen: j ]
  ].

organism getEvolutionStatus displayNl.
