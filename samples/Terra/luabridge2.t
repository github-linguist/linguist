struct A { a : int, b : double }

terra returnstruct()
    var a = A { 1, 2.5}
    return a
end
terra returnstruct2()
    var a = A { 1, 2.5}
    var b = A { 2, 3.5}
    return a,b
end

terra returnarray()
    var a : int[4] = array(1,2,3,4)
    return a
end
terra returnarray2()
    var a : int[4] = array(1,2,3,4)
    var b : int[4] = array(5,6,7,8)
    return a,b
end

terra returnaos()
    var a : A[2] = arrayof(A, {1,2.5}, {2,3.5} )
    return a
end
struct B { a : int[4] }
terra returnsoa()
    var a  = B { array(1,2,3,4) }
    return a
end

global_a = global(A)
global_a:set( {3, 4.5})

terra retstructptr()
    return &global_a
end
terra structptr(a : &A)
    return a.a + a.b
end

global_arr = global(int[2]) 
global_arr:set({3,4})

terra retarrptr()
    return &global_arr
end
terra arrptr(a : &int[2])
    return (@a)[0] + a[0][1]
end

local test = require("test")
local a0 = returnstruct()
test.eq(a0.a,1)
test.eq(a0.b,2.5)
local a1,b1 = terralib.unpackstruct(returnstruct2())
test.eq(a1.a,1)
test.eq(a1.b,2.5)
test.eq(b1.a,2)
test.eq(b1.b,3.5)
--[[ C doesn't actually allow you to return arrays directly
local arr = returnarray()
test.eq(arr[0],1)
test.eq(arr[1],2)
test.eq(arr[2],3)
test.eq(arr[3],4)
local arr2,arr3 = returnarray2()
test.eq(arr2[0],1)
test.eq(arr2[1],2)
test.eq(arr2[2],3)
test.eq(arr2[3],4)
test.eq(arr3[0],5)
test.eq(arr3[1],6)
test.eq(arr3[2],7)
test.eq(arr3[3],8)
local arr4 = returnaos()
test.eq(arr4[0].a,1)
test.eq(arr4[1].b,3.5)
]]

local arr5 = returnsoa()
test.eq(arr5.a[0],1)
test.eq(arr5.a[3],4)
local ptrret = retstructptr()
test.eq(ptrret[0].a,3)
test.eq(ptrret[0].b,4.5)
test.eq(structptr(ptrret),7.5)
local ptrarr = retarrptr()
test.eq(ptrarr[0][0],3)
test.eq(ptrarr[0][1],4)
test.eq(arrptr(ptrarr),7)