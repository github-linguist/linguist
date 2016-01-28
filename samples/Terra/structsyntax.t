struct A {
    a : int
    b : float
    union {
        c : int
        d : float
    }
}

terra foo()
    return A { a = 4, b = 5, c = 3}
end

assert(foo().c == 3)