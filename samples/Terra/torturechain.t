
terra foo()
	return 8.0,9
end

local torturechain = quote
	var a = 3.0
in
	[quote
		var b = 4
	 in
	 	[quote 
	 		var c = 5.0
	 	in
	 		a,b,c, [quote 
	 					var d = 6
	 				in
	 					[quote
	 						var e = 7.0
	 					in
	 						d,e,foo()
	 					end]
	 				end]
	 	end]
	 end]
end

terra bindit()
	var a,b,c,r1 = torturechain
	var d,e,r2 = r1
	var f,g = r2
	return a + b + c + d + e + f + g
end

local sum = 3+4+5+6+7+8+9 
assert(sum == bindit())

terra foo(z : int, a : int, b : int, c : int, d : int, e : int, f : int, g : int)
	return z + a + b + c + d + e + f + g
end


terra overloadit(a : int, b : int, c : int, d : int, e : int, f : int)
	return 0
end

terra overloadit(a : double, b : int, c : int, d : int, e : int, f : int)
	return 1
end


terra callit()
    var a,b,c,r1 = torturechain
	var d,e,r2 = r1
	var f,g = r2
	return foo(0,a,b,c,d,e,f,g)
end

terra callol()
	var a,b,c,r1 = torturechain
	var d,e,r2 = r1
	var f,g = r2
	return overloadit(a,b,c,e,f,g)
end

assert(callit() == sum)
assert(callol() == 1)
