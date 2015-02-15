# Calling a function that requires no arguments
foo()

# Calling a function with a fixed number of arguments
foo 1

# Calling a function with optional arguments
# (Optional arguments are done using an object with named keys)
foo 1, optionalBar: 1, optionalBaz: 'bax'

# Calling a function with a variable number of arguments
# for a function `foo` defined as `foo = ( args... ) ->`
foo 1, 2, 3, 4

# Calling a function with named arguments
# (Named arguments are done using an object with named keys)
foo bar: 1, bax: 'baz'

# Using a function in statement context
x = foo 1

# Using a function in first-class context within an expression
# (For `foo` defined as `foo = ( x ) -> x + 1`
x = [ 1, 2, 3 ].map foo

# Obtaining the return value of a function
x = foo 1

# Arguments are passed by value, even objects. Objects
# are passed as the _value_ of the reference to an object.
# Example:
bar = ( person ) ->
    # Since `person` is a reference
    # to the person passed in, we can assign
    # a new value to its `name` key.
    person.name = 'Bob'

    # Since `person` is just the value of
    # the original reference, assigning to it
    # does not modify the original reference.
    person = new Person 'Frank'

# Partial application is only possible manually through closures
curry = ( f, fixedArgs... ) ->
    ( args... ) -> f fixedArgs..., args...

# Example usage
add = ( x, y ) -> x + y

add2 = curry add, 2

add2 1 #=> 3
