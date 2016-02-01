foo = terra(a : int) end
foo:setname("bar")

assert(foo:getname() == "bar")
foo:disas() 