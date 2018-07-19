Integer extend [
    primesDo: aBlock [
        | div next rest |
        div := 2. next := 3.
        rest := self.
        [ [ rest \\ div == 0 ]
              whileTrue: [
                  aBlock value: div.
                  rest := rest // div ].
          rest = 1] whileFalse: [
              div := next. next := next + 2 ]
    ]
]
123456 primesDo: [ :each | each printNl ]
