# This is a rather obscure technique to have an anonymous
# function call itself.
fibonacci = (n) ->
  throw "Argument cannot be negative" if n < 0
  do (n) ->
      return n if n <= 1
      arguments.callee(n-2) + arguments.callee(n-1)

# Since it's pretty lightweight to assign an anonymous
# function to a local variable, the idiom below might be
# more preferred.
fibonacci2 = (n) ->
  throw "Argument cannot be negative" if n < 0
  recurse = (n) ->
      return n if n <= 1
      recurse(n-2) + recurse(n-1)
  recurse(n)
