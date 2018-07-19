function Strip_Control_Codes( str )
    local s = ""
    for i in str:gmatch( "%C+" ) do
 	s = s .. i
    end
    return s
end

function Strip_Control_and_Extended_Codes( str )
    local s = ""
    for i = 1, str:len() do
	if str:byte(i) >= 32 and str:byte(i) <= 126 then
  	    s = s .. str:sub(i,i)
	end
    end
    return s
end

q = ""
for i = 0, 255 do
	q = q .. string.char(i)
end

print( Strip_Control_Codes(q) )
print( Strip_Control_and_Extended_Codes(q) )
