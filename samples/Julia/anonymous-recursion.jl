function fib(n)
    if n < 0
        throw(ArgumentError("negative arguments not allowed"))
    end
    aux(m) = m < 2 ? one(m) : aux(m-1) + aux(m-2)
    aux(n)
end
