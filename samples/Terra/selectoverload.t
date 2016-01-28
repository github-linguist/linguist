struct Vec { data : float[3] }


local get = {}
get.x = terra(self : &Vec)
    return self.data[0]
end
get.y = macro(function(self)
    return `self.data[1]
end)
    
Vec.metamethods.__entrymissing = macro(function(name,self)
    return `[get[name]](&self) 
end)

terra bar()
    var a = Vec { array(1.f,2.f,3.f) }
    a.y = a.y + 1
    var pa = &a
    return a.x + a.y + pa.x
end

local test = require("test")
test.eq(bar(),5)

