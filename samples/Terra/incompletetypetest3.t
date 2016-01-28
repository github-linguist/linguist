

struct A {
	a : int
}


terra what()
	var a : &A = nil
	return a - a
end

what:compile()