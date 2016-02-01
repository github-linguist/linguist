

function makeloop(N,body)
	return quote
		for i = 0, N do
			body
		end
	end
end


terra stuff()
	var a = 0;
	[makeloop(10,quote 
		a = a + 1 
	end)]
	return a
end


print(stuff())