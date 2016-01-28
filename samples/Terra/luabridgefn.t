
terra plus1(a : int)
    return a + int(1)
end

terra afn() : int -> int
    return plus1
end

terra doit(a : int, b : int -> int)
    return b(a)
end

local test = require("test")

local foobar = afn()
test.eq(foobar(3),4)
test.eq(doit(4,foobar),5)
local function whatwhat(a)
    print("I GOT A ", a)
    return a + 3
end
test.eq(doit(5,whatwhat),8)
