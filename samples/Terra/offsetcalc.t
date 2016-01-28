local offsetinbytescache = {}
function offsetinbytes(structtype,key)
    local typetable = offsetinbytescache[structtype] or {}
    local value = typetable[key]
    if value then
        return value
    end
    offsetinbytescache[structtype] = typetable
    
    local terra offsetcalc() : int
        var a : &structtype = [&structtype](0)
        return [&int8](&a.[key]) - [&int8](a)
    end
    
    local result = offsetcalc()
    
    typetable[key] = result
    return result
end


struct A { a : int8, c : int8, b : int }


terra foo()
	return 4LL
end

local test = require("test")

test.eq(offsetinbytes(A,"b"),4)


