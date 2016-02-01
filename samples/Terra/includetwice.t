

local C1 = terralib.includecstring [[
	typedef struct { int a; } A;
	int geta(A* a) { return a->a; }
]]
local C2 = terralib.includecstring [[
	typedef struct { int a; } A;
	void seta(A* a, int v) { a->a = v; }
]]

assert(C1.A == C2.A)


terra usethem()
	var a = C1.A { 1 }
	C2.seta(&a,C1.geta(&a) + 3)
	return a.a
end

assert(usethem() == 4)