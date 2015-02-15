myclass = setmetatable({
__index = function(z,i) return myclass[i] end, --this makes class variables a possibility
setvar = function(z, n) z.var = n end
}, {
__call = function(z,n) return setmetatable({var = n}, myclass) end
})

instance = myclass(3)

print(instance.var) -->3

instance:setvar(6)

print(instance.var) -->6
