Object subclass: #Example.

Example extend [
  foo: x [
    ^ 42 + x ] ].

symbol := 'foo:' asSymbol. " same as symbol := #foo: "

Example new perform: symbol with: 5. " returns 47 "
