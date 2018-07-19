function compose(f, g) return function(...) return f(g(...)) end end
