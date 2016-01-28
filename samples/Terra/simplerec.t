


local terra bar()
	return 1
end 
and struct A {
	a : int
} 
and local struct B {
	a : int
}
and terra B:foo()
	return self.a
end 
and terra foo()
	var b , a  = B{4}, A{5}
	return 1 + b:foo()
end
and local terra mydecl
and struct mystructdecl

print(bar())

print(foo())
