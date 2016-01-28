function mymacro()
    return {`4,`5}
end
mymacro = macro(mymacro)

local exps = {`2,`3, `mymacro()}

terra doit()
    return exps
end
local test = require("test")
test.meq({2,3,4,5},doit())