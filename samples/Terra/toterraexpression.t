local x = setmetatable({a = 4}, { __toterraexpression = function(self) return self.a  end})


local terra foo()
    return x
end

assert(foo() == 4)