function makecalcfn(inst)    
    local stk = {}
    for i,v in ipairs(inst) do
        if type(v) == "number" then
            table.insert(stk,`v)
        else
            local b = table.remove(stk)
            local a = table.remove(stk)
            
            if v == "+" then
                table.insert(stk,`a + b)
            elseif v == "-" then
                table.insert(stk,`a - b)
            elseif v == "*" then
                table.insert(stk,`a * b)
            elseif v == "/" then
                table.insert(stk,`a / b)
            end
        end
    end
    
    local result = table.remove(stk)
    
    local terra wrapper()
        return result
    end
    
    return wrapper
end

local calcfn = makecalcfn({5,4,"*",5,"-",3,"+"})
local test = require("test")
test.eq(calcfn(),18)
