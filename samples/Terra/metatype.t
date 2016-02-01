local function foo(T)
    terra T:what()
        return self.a + self.b
    end
end
struct A(foo) {
    a : int;
    b : int;
}

terra test()
    var a  = A {1,2}
    return a:what()
end

assert(test() == 3)