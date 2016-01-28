
local bar = terralib.internalmacro(function(ctx,tree,typ)
	return terralib.newtree(typ.tree, { kind = terralib.kinds.literal, type = double, value = 4.0 })
	
end)

local bar2 = macro(function(typ)
	return typ
	
end)


local bar3 = macro(function(a,b)
    return {a.tree,b.tree}
end)

terra up(v : &int)
    @v = @v + 1
end

local bar4 = macro(function()
    local terra myfn()
        return 42
    end
    return myfn
end)

moo = global(int,3)

local bar4 = macro(function()
    local terra myfn()
        return 42
    end
    return myfn
end)

local bar5 = macro(function()
    return moo
end)

terra foo() : int
	var a : int = bar(int,int16,int32)
	bar2(a) = bar2(a) + 5
	bar3(up(&a),up(&a))
	bar5() = bar5() + 1
	return a + bar4()() + moo
end

local test = require("test")
test.eq(57,foo())
