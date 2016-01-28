function failit(fn,err)
	local success,msg = pcall(fn)
	if success then
		error("failed to fail.",2)
	elseif not string.match(msg,err or "Errors reported during") then
		error("failed wrong: "..msg,2)
	end
end


failit(function()
local A = {}
terra A:foo() end
end,"expected a struct")


failit(function()
terra A.b() end
end,"failed attempting to index field")

failit(function()
terra A.b.c() end
end,"failed attempting to index field")


failit(function()
local A = 4
struct A.b {}
end,"failed attempting to index field")