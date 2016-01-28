
struct A { a : &A, b : int }

terra foo(a : A)
    return a.b
end

local test = require("test")

test.eq(foo({b = 5}),5)
