
local mymacro = macro(function(a)
    a = a:asvalue()
    print(a, terralib.issymbol(a), terralib.unsafetypeofsymbol(a))
    return {}
end)



terra foo(a : int)
    mymacro(a)
end

foo:compile()