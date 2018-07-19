h1 = {foo: "bar"}
h2 = {foo: "bar"}

true_expressions = [
  true
  1
  h1? # because h1 is defined above
  not false
  !false
  []
  {}
  1 + 1 == 2
  1 == 1 # simple value equality
  true or false
]

false_expressions = [
  false
  not true
  undeclared_variable?
  0
  ''
  null
  undefined
  h1 == h2 # despite having same key/values
  1 == "1" # different types
  false and true
]
