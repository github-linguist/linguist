function simpson(f, a,b, n)
    h = (b-a)/n
    s = f(a + h/2)
    for i in 1:(n-1)
        s += f(a + h*i + h/2) + 0.5 * f(a + h*i)
    end
    h/6 * (f(a) + f(b) + 4*s)
end
