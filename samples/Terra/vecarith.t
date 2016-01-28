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


N = 2

types = {int,int64,uint64,float,double}
names = {"i","l","u","f","d"}
max = 16

for i = 1,#types do
    local fenv = getfenv()
    for j = 0,max do
        local x  = global(vector(types[i],N));
        (terra() x = j end)()
        fenv[names[i]..j] = x
    end
end


d375 = global(vector(double,N))
d175 = global(vector(double,N))
terra init()
    d375 = 3.75
    d175 = 1.75
end
init()

terra test0()
    var a = i1 + i3
    var b = l1 + l2
    var c = u1 + u2
    var d = f1 + f2
    var e = d1 + d2
    return (a + b + c + d + e)[1]
end

terra test1()
    var a = i1 - i3
    var b = l1 - l2
    var c = u1 - u2
    var d = f1 - f2
    var e = d1 - d2
    return (a - b - c - d - e)[1]
end

terra test2()
    var a = i2 * i3
    var b = l3 * l2
    var c = u3 * u2
    var d = f1 * f2
    var e = d3 * d2
    return (a * b * c * d * e)[1]
end

terra test3()
    var a = i2 / i3 + i1
    var b = l3 / l2 + i1
    var c = u3 / u2
    var d = f1 / f2
    var e = d3 / d2
    return (a * b * c * d * e)[1]
end

terra test4()
    var a = i2 % i3
    var b = l3 % l4
    var c = u3 % u2
    var d = f1 % f2
    var e = d375 % d2
    return ((a == i2) and (b == l3) and (d == f1) and (e == d175))[1]
end

terra test5()
    var a = i2 ^ i3
    var b = l3 ^ l4
    var c = u3 ^ u2
    return ((a == 1) and (b == l7) and (c == u1))[1]
end

terra test6()
    var a = i2 and i3
    var b = l3 and l4
    var c = u3 and u2
    return ((a == 2) and (b == l0) and (c == u2))[1]
end

terra test7()
    var a = i2 or i3
    var b = l3 or l4
    var c = u3 or u2
    return ((a == 3) and (b == l7) and (c == u3))[1]
end


terra test8()
    var a0,a1 = i2 ~= i3, i2 == i3
    var b0,b1 = i2 < i3, i2 >= i3
    var c0,c1 = i2 > i3, i2 <= i3
    
    return (a0 and not a1 and b0 and not b1 and not c0 and c1)[1]
end

terra test9()
    var a0, a1 = i8 >> i1, i8 << i1
    var b0, b1 = -i8 >> i1, -i8 << i1
    return (a0 == i4 and a1 == i16 and b0 == -i4 and b1 == -i16)[1]
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