#assert() function takes expression as 1st argument, failed-assertion message as optional 2nd argument
julia> assert(x==42,"x is not 42")
ERROR: assertion failed: x is not 42
#@assert macro checks the supplied conditional expression, with the expression returned in the failed-assertion message
julia> @assert x==42
ERROR: assertion failed: :((x==42))
#Julia also has type assertions of the form, x::Type which can be appended to a variable for type-checking at any point
julia> x::String
ERROR: type: typeassert: expected String, got Int32
