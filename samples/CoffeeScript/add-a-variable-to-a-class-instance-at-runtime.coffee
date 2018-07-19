# CoffeeScript is dynamic, just like the Javascript it compiles to.
# You can dynamically add attributes to objects.

# First create an object very simply.
e = {}
e.foo = "bar"
e.yo = -> "baz"
console.log e.foo, e.yo()

# CS also has class syntax to instantiate objects, the details of which
# aren't shown here.  The mechanism to add members is the same, though.
class Empty
  # empty class

e = new Empty()
e.foo = "bar"
e.yo = -> "baz"
console.log e.foo, e.yo()
