
struct A { a : int }

terra A.methods.foo(a : int)
	return a + 1
end

function A.metamethods.__cast(from,to,exp)
	if from == A and to == int then
		return `exp.a
	end
	error("what")
end



terra testit()
	var a = A { 5 }
	return a:foo()
end

assert(6 == testit())