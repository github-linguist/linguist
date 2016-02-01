

local S = terralib.types.newstruct("mystruct")

struct A {
	v : int
}

S.entries:insert( { type = A, field = "what" } )
terra foo()
	var v : S
	return v.what.v
end

foo:compile()
