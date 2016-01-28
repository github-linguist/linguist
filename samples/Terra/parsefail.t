
local self = 1
local Rt = 1
local i = 1
local j = 1
terra bar()
var a = Rt.MatrixDouble{[&double](self.ptr) , i,j}
end

local r,e = terralib.loadstring[[
	
	terra foo()
		var a = { [&double](4) = 3 }
	end
]]

assert(r == nil and e:match("unexpected symbol near '='"))

terra foo()
	var a = { [""] = 3 }
end

local s = symbol()

local function getsym()
	return s
end
terra foo2()
	var [getsym()] = 3
	var a = { [getsym()] = 4, _1 = [getsym()] }
	return a.[getsym()] + a._1
end

assert(7 == foo2())

