local Vec = terralib.memoize(function(typ,N)
    N = assert(tonumber(N),"expected a number")
    local ops = { "__sub","__add","__mul","__div" }
    local struct VecType { 
        data : typ[N]
    }
    VecType.metamethods.type, VecType.metamethods.N = typ,N
    VecType.metamethods.__typename = function(self) return ("%s_%d"):format(tostring(self.metamethods.type),self.metamethods.N) end
    for i, op in ipairs(ops) do
        local i = symbol("i")
        local function template(ae,be)
            return quote
                var c : VecType
                for [i] = 0,N do
                    c.data[i] = operator(op,ae,be)
                end
                return c
            end
        end
        local terra doop(a : VecType, b : VecType) [template(`a.data[i],`b.data[i])]  end
        terra doop(a : typ, b : VecType) [template(`a,`b.data[i])]  end
        terra doop(a : VecType, b : typ) [template(`a.data[i],`b)]  end
       VecType.metamethods[op] = doop
    end
    terra VecType.methods.FromConstant(x : typ)
        var c : VecType
        for i = 0,N do
            c.data[i] = x
        end
        return c
    end
    VecType.metamethods.__apply = macro(function(self,idx) return `self.data[idx] end)
    VecType.metamethods.__cast = function(from,to,exp)
        if from:isarithmetic() and to == VecType then
            return `VecType.FromConstant(exp)
        end
        error(("unknown conversion %s to %s"):format(tostring(from),tostring(to)))
    end
    return VecType
end)

terra foo(v : Vec(float,4), w : Vec(float,4))
    var z : Vec(float,4) = 1
    var x = (v*4)+w+1
    for i = 0,4 do
        print(x(i))
    end
    return x(2)
end

foo:printpretty(true,false)
foo:disas()

assert(20 == foo({{1,2,3,4}},{{5,6,7,8}}))