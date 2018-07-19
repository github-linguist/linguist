function print_floyd(rows)
	local c = 1
	local h = rows*(rows-1)/2
	for i=1,rows do
		local s = ""
		for j=1,i do
			for k=1, #tostring(h+j)-#tostring(c) do
				s = s .. " "
			end
			if j ~= 1 then s = s .. " " end
			s = s .. tostring(c)
			c = c + 1
		end
		print(s)
	end
end

print_floyd(5)
print_floyd(14)
