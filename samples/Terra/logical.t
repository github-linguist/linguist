

local T = true
local F = false

terra check()
    var a = true
    return not true == false
end

assert(check() == true)

terra check2()
    return not T == F
end

assert(check2() == true)

terra check3()
    return not T ==  not not T
end

assert(check3() == false)

terra check4()
    return not not T == T and not not F == F and true == T and false == F 
end

assert(check4() == true)

terra foo() return not false end
 
assert(foo() == true) 