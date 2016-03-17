--[[

-
+

*
/
%

^
and
or
~= == < > >= <=
<< >>

]]




terra test0()
    var a = 1 + 3
    var b = 1LL + 2LL
    var c = 1ULL + 2ULL
    var d = 1.f + 2.f
    var e = 1.0 + 2.0
    return a + b + c + d + e
end
terra test1()
    var a = 1 - 3
    var b = 1LL - 2LL
    var c = 1ULL - 2ULL
    var d = 1.f - 2.f
    var e = 1.0 - 2.0
    return a - b - c - d - e
end
terra test2()
    var a = 2 * 3
    var b = 3LL * 2LL
    var c = 3ULL * 2ULL
    var d = 1.f * 2.f
    var e = 3.0 * 2.0
    return a * b * c * d * e
end

terra test3()
    var a = 2 / 3 + 1
    var b = 3LL / 2LL + 1
    var c = 3ULL / 2ULL
    var d = 1.f / 2.f
    var e = 3.0 / 2.0
    return a * b * c * d * e
end

terra test4()
    var a = 2 % 3
    var b = 3LL % 4LL
    var c = 3ULL % 2ULL
    var d = 1.f % 2.f
    var e = 3.75 % 2.0
    return (a == 2) and (b == 3LL) and (d == 1.f) and (e == 1.75)
end

terra test5()
    var a = 2 ^ 3
    var b = 3LL ^ 4LL
    var c = 3ULL ^ 2ULL
    return (a == 1) and (b == 7LL) and (c == 1ULL)
end

terra test6()
    var a = 2 and 3
    var b = 3LL and 4LL
    var c = 3ULL and 2ULL
    return (a == 2) and (b == 0LL) and (c == 2ULL)
end

terra test7()
    var a = 2 or 3
    var b = 3LL or 4LL
    var c = 3ULL or 2ULL
    return (a == 3) and (b == 7LL) and (c == 3ULL)
end


terra test8()
    var a0,a1 = 2 ~= 3, 2 == 3
    var b0,b1 = 2 < 3, 2 >= 3
    var c0,c1 = 2 > 3, 2 <= 3
    
    return a0 and not a1 and b0 and not b1 and not c0 and c1
end

terra test9()
    var a0, a1 = 8 >> 1, 8 << 1
    var b0, b1 = -8 >> 1, -8 << 1
    return a0 == 4 and a1 == 16 and b0 == -4 and b1 == -16
end


local test = require("test")

test.eq(test0(),16)
test.eq(test1(),2)
test.eq(test2(),2592)
test.eq(test3(),1.5)

test.eq(test4(),true)

test.eq(test5(),true)
test.eq(test6(),true)
test.eq(test7(),true)
test.eq(test8(),true)
test.eq(test9(),true)