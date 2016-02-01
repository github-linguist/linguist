
--this is a comment.
--top level is Lua code:
function min(a,b) 
	if a < b then return a
	else return b end
end

print(min(3,4)) --3

terra mint(a : int, b : int) : int
if a < b then return a
else return b end
end

print(mint(3,4)) --3

mint:disas()