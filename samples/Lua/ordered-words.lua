fp = io.open( "dictionary.txt" )

maxlen = 0
list = {}

for w in fp:lines() do
    ordered = true
    for l = 2, string.len(w) do
	if string.byte( w, l-1 ) > string.byte( w, l ) then
	    ordered = false
  	    break
	end
    end
    if ordered then
	if string.len(w) > maxlen then
	    list = {}
	    list[1] = w
	    maxlen = string.len(w)
	elseif string.len(w) == maxlen then
	    list[#list+1] = w
	end
    end
end

for _, w in pairs(list) do
    print( w )
end

fp:close()
