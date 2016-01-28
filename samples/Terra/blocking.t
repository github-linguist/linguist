function symmat(name,I,...)
	if not I then return symbol(name) end
	local r = {}
	for i = 1,I do
		r[i] = symmat(name..tostring(i),...)
	end
	return r
end

terra min(a : int, b : int)
	return terralib.select(a < b, a, b)
end

function blockedloop(bounds,sizes,bodyfn)
	local indexes = symmat("i",#sizes,#bounds)
	--local makeloop --bug local function doesn't add to set of live variables...
	local function makeloop(s,b)
			if s > #sizes then
				return bodyfn(unpack(indexes[#sizes]))
			elseif b > #bounds then
				return makeloop(s + 1, 1)
			else
				local topbound = bounds[b]
				local blocksize = sizes[s]
				local begin,bound
				if s == 1 then
					begin,bound = 0, topbound
				else
					begin,bound = indexes[s-1][b], sizes[s-1]
				end
				local step = sizes[s]
				return quote
					for [indexes[s][b]] = begin,min(begin+bound,topbound),step do
						[ makeloop(s,b+1) ]
					end
				end
			end
	end
	return makeloop(1,1)
end

IO = terralib.includec("stdio.h")

terra main()
	
	var M,N = 30,40;

	[blockedloop({M,N}, {10,1}, function(m,n)
		return quote 
			IO.printf("%d %d\n",m,n)
		end
	end)]
	
end

main:printpretty()
main()