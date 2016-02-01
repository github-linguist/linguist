local c = terralib.includec("stdlib.h")

new = macro(function(typquote)
    local typ = typquote:astype()
    return `[&typ](c.malloc(sizeof(typ)))
end)

local typ = int
terra doit()
    var a : &int = new(int)
    return a
end

doit()