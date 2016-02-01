local test = require("test")

terra foo(a : int)
    return 1 << 2, a >> 1, -4 >> 1, uint32(-a) >> 1
end

test.meq({4,2,-2,2147483646},foo(4))
