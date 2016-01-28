
local test = require("test")

function mkspecial(N)
    local terra pow(a : double)
        var i,r = 0,1.0
        while i < N do
            r = r * a
            i = i + 1
        end
        return r
    end
    return pow
end


local pow2 = mkspecial(2)
local pow3 = mkspecial(3)

test.eq(pow2(2),4)
test.eq(pow3(2),8)


function mkor(T)
    local terra fn(a : T, b : T) : T
        return a or b
    end
    return fn
end

local lor = mkor(bool)
local aor = mkor(int)

test.eq(lor(1,2),true)
test.eq(aor(1,2),3)


--[[
function my_lua_fun()
    
    val my_list = new_list()
    defer delete(my_list)
    
    

    return { ["a"] = 1, ["b"] = 2 }
end
--wess weimer PhD on controlling actions
a:my_method(b)

a.my_method(a,b)

var a : T
a:my_method(b)
T.my_method(a,b)

Pair = typedef(struct { a = int, b = int})

var a : Pair = { 1, 3 }
var a : Pair = my_lua_fun()
]]