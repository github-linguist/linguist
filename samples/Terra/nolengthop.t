local a,b = terralib.loadstring([[terra g() return #a]])
assert(b:find("operator not supported"))