compose(f::Function, g::Function) = x->f(g(x))
