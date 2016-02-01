

terra foo(a : int, b : int)
    return a + b
end

local ffi =  require 'ffi'

local name = (ffi.os == "Windows" and "foo.dll" or "foo.so")

local args = {}

if ffi.os == "Windows" then
	args = {"/IMPLIB:foo.lib","/EXPORT:foo" }
end

terralib.saveobj(name,{ foo = foo }, args)

local foo2 = terralib.externfunction("foo", {int,int} -> int )
terralib.linklibrary("./"..name)

assert(4 == foo2(1,3))
