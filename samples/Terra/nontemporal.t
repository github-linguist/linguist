

terra foobar(a : &vector(float,4),b : vector(int,4))
	terralib.attrstore(a,b,{ nontemporal = true })
end

foobar:disas()
