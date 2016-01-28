C = terralib.includec("stdio.h")


cur = global(int)

terra d(a : int)
    if cur ~= a then
        C.printf("found %d while expecting %d\n",a,cur)
        error(cur)
    end
    cur = cur + 1
    --C.printf("d%d\n",a)
end
d:setinlined(false)

terra side(a : int, b : {})
    d(a)
end

terra doit()
    d(0)
    defer side(3,d(1))
    d(2)
end
cur:set(0)
doit()


terra doit2()
    d(0)
    var i = 100
    defer d(203)
    repeat
        defer d(2*(100-i)+2)
        d(2*(100-i)+1)
        i = i - 1
    until i == 0
    defer d(202)
    do defer d(201) end
end
cur:set(0)
doit2()

terra doit3()
    d(0)
    defer d(11)
    for i = 0,10 do
        defer d(i+1)
    end
end

cur:set(0)
doit3()

terra doit4()
    d(0)
    defer d(5)
    if true then
        defer d(2)
        d(1)
    end
    if false then
    else
        defer d(3)
    end
    d(4)
end
cur:set(0)
doit4()

struct A {
}

A.methods.stackalloc = macro(function()
    return quote
        var a : A
        defer a:free()
    in
        &a
    end
end)
terra A:free()
    d(1)
    --C.printf("freeing A (%p)\n",self)
end
terra A:print()
    d(0)
    --C.printf("printing A (%p)\n",self)
end
terra doit5()
    var a = A.stackalloc()
    a:print()
end
cur:set(0)
doit5()

terra doit6(a : int)
    defer d(2)
    d(0)
    if a == 0 then
        defer d(1)
        return 4
    end
    d(1)
    return 3
end
cur:set(0)
doit6(0)

cur:set(0)
doit6(1)
