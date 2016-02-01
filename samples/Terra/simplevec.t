
terra sum(input : &float, N : int)
	var acc : vector(float,4) = vector(0.f,0.f,0.f,0.f)
	for i = 0,N,4 do
		--cast the floats to float4s and load
		var entry = @[&vector(float,4)](input + i)
		acc = acc + entry
	end
	return acc[0] + acc[1] + acc[2] + acc[3]
end

sum:disas()
