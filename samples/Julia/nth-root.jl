function nthroot(n::Integer, A::Real)
    A < 0 || n == 0 && throw(DomainError())
    n < 0 && return 1/nthroot(-n, A)
    A == 0 && return A
    x = A / n
    prevdx = A
    while true
        y = x^(n-1)
        dx = (A - y*x) / (n * y)
        abs(dx) >= abs(prevdx) && return x
        x += dx
        prevdx = dx
    end
end
@vectorize_2arg Number nthroot
