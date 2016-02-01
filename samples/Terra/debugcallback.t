
terralib.settypeerrordebugcallback(function(o)
    o:printpretty()
end)


local function dosomecomplicatedstuff(a)
    return `@a
end

local s,e = pcall(function()

    local terra what(a : &opaque)
        var b = [dosomecomplicatedstuff(a)]
        return b
    end
    what:compile()

end)

assert(not s)
print(e)
assert(e:match("Errors reported during"))