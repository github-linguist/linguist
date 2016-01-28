terra f(x :  {})
	return x
end

print(f({}))

terra f2(x :  {})
	return x,x
end

print(f2({}))

terra f3(x : {}, a : int)
	return a + 1
end

assert(f3({},3) == 4)

terra f4(x :  {})
	return x,4
end

a = f4({})
assert(a._1 == 4)

terra f5()
	return f({})
end

f5()