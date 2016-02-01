

struct A {
	a : int
}

count = global(int,0)

terra twoAs(a : int)
	return A {a}, A { a }
end

function A.metamethods.__cast(fromt,tot,exp)
	if tot == A and fromt == int then
		return `twoAs(exp)._0
	end
	error("what")
end

terra twoInts()
	count = count + 1
	return count,2
end

terra takesAnA(a : A)
	return a.a
end

dotwice = macro(function(exp)
	return {exp,exp}
end)

terra doit()
	return dotwice(takesAnA((twoInts()._0))) 
end


doit:printpretty()
local test = require("test")
test.meq({1,2},doit())
