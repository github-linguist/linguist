
struct A { a : int, b : float }

function A.metamethods.__cast(from,to,exp)
    if from == int and to == A then
        return `A {exp, 1.f }
    elseif from == float and to == A then
        return `A { 1, exp }
    end
    error("invalid")
end


terra moo(a : A)
    return a.a + a.b
end

terra bar()
    return moo(1) + moo(1.f)
end

local test = require("test")

test.eq(bar(), 4)