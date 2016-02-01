

struct Foo {
    a : int;
    b : int;
}

terra Foo:bar()
    self.a = self.a + 1
    return self.a + self.b
end

Foo.methods.baz = function(self) print(self.a,self.b) end
Foo.methods.mac = macro(function() return `1 end)

terra usemethod()
    var a = Foo { 3, 4}
    a:baz()
    return a:bar()
end
assert(8 == usemethod())

obj = terralib.new(Foo[1], {{3,4}})

assert(8 == obj[0]:bar())
assert(9 == obj[0]:bar())
obj[0]:baz()

local a,b = pcall(function()
obj[0]:mac()
end)
assert(not a)
assert(string.match(b,"not supported"))

local a,b = pcall(function()
obj[0]:maz()
end)

assert(not a)
assert(string.match(b,"attempt to call method"))