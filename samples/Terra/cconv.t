
local test = require("test")

terra f1(a : int)
	return a
end

terra c1()
	return 1 + f1(4),f1(4)
end

terra f2(a : int, b : float)
	return a + b
end

terra c2()
	return f2(3,4) + 1, f2(3,4)
end


terra f3()
	return 3,4
end

terra c3()
    var r = f3()
	return f3()._0 + 1,unpackstruct(r) 
end

terra f4() : {float,float}
	return 3.25,4.25
end

terra c4()
    var r = f4()
	return f4()._0 + 1, unpackstruct(r) 
end


terra f5(a : int) : {uint8,uint8,uint8,uint8}
	return 0,1,a,3
end

terra c5()
    var r = f5(8)
	return f5(8)._0 + 1, unpackstruct(r)
end

terra f6() : {double, double, int}
	return 3.25, 4.25, 3
end

terra c6()
    var r = f6()
	return f6()._0 + 1, unpackstruct(r)
end
test.meq({4.25, 3.25,4.25,3},c6())

terra f7(a : int) : {double, double, int}
	return 3.25, 4.25, a
end

terra c7()
    var r = f7(4)
	return f7(4)._0 + 1, unpackstruct(r)
end
test.meq({4.25,3.25,4.25,4},c7())

terra f8() : {double, double}
	return 3.25, 4.25
end

terra c8()
    var r= f8()
	return f8()._0 + 1, unpackstruct(r)
end

test.meq({4.25,3.25,4.25},c8())

struct S1 {
	a : int;
	b : int;
}

terra f9(a : S1)
	return a.a+1,a.b+2
end

terra c9()
	var a = S1 { 4, 5}
	var r = f9(a)
	return f9(a)._0 + 1, unpackstruct(r)
end
test.meq({6,5,7},c9())

struct S2 {
	a : int;
	b : double;
	c : double;
}

terra f10(a : S2)
	return a.a, a.b, a.c
end

terra c10()
	var s2 = S2 { 4,5,6 }
	var r = f10(s2)
	return f10(s2)._0 + 1, unpackstruct(r) 
end

test.meq({5,4,5,6},c10())

C = terralib.includec("stdio.h")

terra f11(a : int)
	C.printf("f11 %d\n",a)
end


terra c11()
	f11(7)
end
c11()

struct S3 {
	a : vector(float,2);
	b : double;
}

struct S4 {
	b : double;
	a : vector(float,2);
}

struct S5 {
	a : vector(uint8,4);
	b : int;
}

terra f12a(a : S3) 
	return a.a[0] + a.a[1], a.b
end

terra c12a()
	var a = S3 { vector(2.25f, 3.25f), 4 }
	var r = f12a(a)
	return f12a(a)._0 + 1, unpackstruct(r)
end
test.meq({6.5,5.5,4},c12a())

terra f12b(a : S4) 
	return a.a[0] + a.a[1], a.b
end

terra c12b()
	var a = S4 { 4, vector(2.25f, 3.25f) }
	var r = f12b(a)
	return f12b(a)._0 + 1, unpackstruct(r)
end
test.meq({6.5,5.5,4},c12b())


terra f12()
	var a = S3 { vector(8.f,2.f), 3.0 }
	var b = S4 {  3.0, vector(8.f,2.f) }
	var c,d = f12a(a)
	var e,f = f12b(b)
	return c,d,e,f
end

terra f13a(a : S5)
	return a.a[0] + a.a[1] + a.a[2] + a.a[3], a.b
end

terra f13()
	var a = S5 { vectorof(int8, 1,2,3,4), 5 }
	return f13a(a)
end


struct S6 {
	a : float;
	aa : float;
	b : float
}

struct S7a {
	a : int;
	b : int;
}
struct S7 {
	a : int;
	b : S7a;
	c : int;
}

terra f14(a : S6)
	return a.a,a.aa,a.b
end

terra c14()
	var a = S6 { 4,2,3}
	var r = f14(a)
	return f14(a)._0 + 1, unpackstruct(r)
end
test.meq({5,4,2,3},c14())

terra f15(a : S7)
	return a.a, a.b.a, a.b.b, a.c
end

terra c15()
	var a = S7 {1, S7a { 2,3 }, 4}
	var r = f15(a)
	return f15(a)._0 + 1, unpackstruct(r)
end

test.meq({2,1,2,3,4}, c15())

struct S8 {
	a : uint8[7];
}

terra f16(a : S8)
	return a.a[0],a.a[6]
end

terra c16()
	var a = S8 { arrayof(uint8, 1,2,3,4,5,6,7) }
	var r = f16(a)
	return f16(a)._0 + 1, unpackstruct(r)
end

test.meq({2,1,7},c16())

struct S9 {
	a : uint8[9];
}

terra f17(a : S9)
	return a.a[0],a.a[8]
end

terra c17()
	var a = S9 { arrayof(uint8, 1,2,3,4,5,6,7,8,9) }
	var r = f17(a)
	return f17(a)._0 + 1, unpackstruct(r)
end


test.meq({2,1,9},c17())


struct S10 {
	a : double;
	b : int64
}


terra f18a(a0 : int, a1 : int, a2 : int, a3 : int, a4: int, a5 : int, a : S10)
	return a.a, a.b
end

terra c18a()
    var r = f18a(1,2,3,4,5,6,S10{7,8})
	return f18a(1,2,3,4,5,6,S10{7,8})._0 + 1, unpackstruct(r)
end

test.meq({8,7,8},c18a())


terra f18b(a0 : int, a1 : int, a2 : int, a3 : int, a4: int, a : S10)
	return a.a, a.b
end

terra c18b()
    var r = f18b(1,2,3,4,5,S10{7,8})
	return f18b(1,2,3,4,5,S10{7,8})._0 + 1, unpackstruct(r)
end

test.meq({8,7,8},c18b())

terra f18c(a0 : int, a1 : int, a2 : int, a3 : int, a4: int, a : S10)
	return a.a, a.b, a0, a1, a2
end
terra c18c()
    var r = f18c(1,2,3,4,5,S10 {7,8})
	return f18c(1,2,3,4,5,S10{7,8})._0 + 1, unpackstruct(r)
end

test.meq({8,7,8,1,2,3},c18c())

struct S11 {
	a : float;
	b : int;
}

terra f18d(a0 : int, a1 : int, a2 : int, a3 : int, a4: int, a5 : int, a : S11)
	return a.a, a.b
end
terra c18d()
    var r = f18d(1,2,3,4,5,6,S11{7,8})
	return f18d(1,2,3,4,5,6,S11{7,8})._0 + 1, unpackstruct(r)
end
test.meq({8,7,8},c18d())


terra f18e(a0 : int, a1 : int, a2 : int, a3 : int, a4: int, a : S11)
	return a.a, a.b
end

terra c18e()
    var r = f18e(1,2,3,4,5,S11{7,8})
	return f18e(1,2,3,4,5,S11{7,8})._0 + 1, unpackstruct(r) 
end

test.meq({8,7,8},c18e())

terra f18f(a0 : int, a1 : int, a2 : int, a3 : int, a4: int, a : S11)
	return a.a, a.b, a0, a1, a2
end

terra c18f()
    var r = f18f(1,2,3,4,5,S11{7,8})
	return f18f(1,2,3,4,5,S11{7,8})._0 + 1, unpackstruct(r)
end

test.meq({8,7,8,1,2,3},c18f())


terra f18g(a0 : float, a1 : float, a2 : float, a3 : float, a4: float, a5 : float, a6 : float, a7 : float, a : S10)
	return a.a, a.b
end

terra c18g()
    var r = f18g(1,2,3,4,5,6,9,10,S10{7,8})
	return f18g(1,2,3,4,5,6,9,10,S10{7,8})._0 + 1, unpackstruct(r)
end

test.meq({8,7,8},c18g())

terra f18h(a0 : float, a1 : float, a2 : float, a3 : float, a4: float, a5 : float, a6 : float, a : S10)
	return a.a, a.b
end

terra c18h()
    var r = f18h(1,2,3,4,5,6,9,S10{7,8})
	return f18h(1,2,3,4,5,6,9,S10{7,8})._0 + 1, unpackstruct(r)
end

test.meq({8,7,8},c18h())

terra f18i(a0 : float, a1 : float, a2 : float, a3 : float, a4: float, a5 : float, a6 : float, a : S10)
	return a.a, a.b, a0, a1, a2
end

terra c18i()
    var r = f18i(1,2,3,4,5,6,9,S10{7,8})
	return f18i(1,2,3,4,5,6,9,S10{7,8})._0 + 1, unpackstruct(r)
end

test.meq({8,7,8,1,2,3},c18i())

struct S12 {
	a : float;
	b : int;
}

terra f19(a : S12) 
	return a.a, a.b
end
terra c19()
	var a = S12 { 3,5 }
	var r = f19(a)
	return f19(a)._0 + 1, unpackstruct(r)
end
test.meq({4,3,5},c19())



terra f20(a : S10, b : int)
	return a.a,a.b,b
end
terra c20()
    var r = f20(S10{1,2},3)
	return f20(S10{1,2},3)._0 + 1, unpackstruct(r)
end
test.meq({2,1,2,3},c20())


terra f21()
	return
end
f21()

terra f22()
	return S12 { 3, 4}
end
terra c22()
	return f22().a, f22()
end
local s22_0, s22_1 = terralib.unpackstruct(c22())
test.eq(s22_0,3)
test.eq(s22_1.a,3)
test.eq(s22_1.b,4)

terra f23()
	return S10 { 1, 2}
end

terra c23()
	return f23().a, f23()
end
local s23_0, s23_1 = terralib.unpackstruct(c23())
test.eq(s23_0,1)
test.eq(s23_1.a,1)
test.eq(s23_1.b,2)

terra f24()
	return S2 { 1,2,3}
end

terra c24()
	return f24().a, f24()
end
local s24_0, s24_1 = terralib.unpackstruct(c24())
test.eq(s24_0,1)
test.eq(s24_1.a,1)
test.eq(s24_1.b,2)
test.eq(s24_1.c,3)


local s22 = f22()
test.eq(s22.a,3)
test.eq(s22.b,4)

local s23 = f23()
test.eq(s23.a,1)
test.eq(s23.b,2)


local s24 = f24()

test.eq(s24.a,1)
test.eq(s24.b,2)
test.eq(s24.c,3)


test.meq({1,2,3},f20({1,2},3))


test.eq(f1(3),3)
test.eq(f2(4,5),9)
test.meq({3,4},f3())
test.meq({3.25,4.25},f4())
test.meq({0,1,2,3},f5(2))
test.meq({3.25,4.25,3},f6())
test.meq({3.25,4.25,4},f7(4))
test.meq({3.25,4.25},f8())
test.meq({3,5},f9({2,3}))
test.meq({1,2.5,3.5},f10({1,2.5,3.5}))
f11(3)
test.meq({10,3,10,3},f12())
test.meq({10,5},f13())
test.meq({4,5,6},f14({4,5,6}))
test.meq({1,2,3,4},f15({1,{2,3},4}))
test.meq({1,7},f16({{1,2,3,4,5,6,7}}))
test.meq({1,9},f17({{1,2,3,4,5,6,7,8,9}}))
test.meq({7,8},f18a(1,2,3,4,5,6,{7,8}))
test.meq({7,8},f18b(1,2,3,4,5,{7,8}))
test.meq({7,8,1,2,3},f18c(1,2,3,4,5,{7,8}))

test.meq({7,8},f18d(1,2,3,4,5,6,{7,8}))
test.meq({7,8},f18e(1,2,3,4,5,{7,8}))
test.meq({7,8,1,2,3},f18f(1,2,3,4,5,{7,8}))

test.meq({9,10},f18g(1,2,3,4,5,6,7,8,{9,10}))
test.meq({9,10},f18h(1,2,3,4,5,6,7,{9,10}))
test.meq({9,10,1,2,3},f18i(1,2,3,4,5,6,7,{9,10}))

test.meq({4,5}, f19({4,5}))

test.meq({5,4},c1())
test.meq({8,7},c2())
test.meq({4,3,4},c3())
test.meq({4.25,3.25,4.25},c4())
test.meq({1,0,1,8,3},c5())