local c = terralib.includec("stdio.h")

iamclean = macro(function(arg)
    return quote
        var a = 3
        return a,arg
    end
end)
    
terra doit()
    var a = 4
    iamclean(a)
end

local a = doit()
local test = require("test")
test.meq({3,4}, a)