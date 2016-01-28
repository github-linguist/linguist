local result,err = terralib.loadstring [[
terra result()
    [startXNeeded] = a + strip*L.stripWidth
    [endXNeeded] = 1
end
]]
assert(result ~= nil)