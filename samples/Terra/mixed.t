a = terra() return 4 end
local d = {}
do 
    struct A{}
    and local terra a() return 5 end
    and terra d.d(a : B) end
    and terra B:a() end
    and struct B {}
    and terra c() end
    and struct C {}
    assert(a() == 5)
end
assert(a() == 4)