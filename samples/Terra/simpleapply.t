struct Vec { data : int[4] }
Vec.metamethods.__apply = terra(self : &Vec, i : int)
    return self.data[i]
end

struct Vec2 { data : int[4] }
Vec2.metamethods.__apply = macro(function(self,b)
    return `self.data[b]
end)

terra bar()
    var a = Vec { array(1,2,3,4) }
    var b = Vec2 { array(1,2,3,4) }
    b(2) = b(2) + 1
    return b(2) + a(2)
end

local test = require("test")
test.eq(bar(),7)