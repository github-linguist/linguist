function zeck(n)
    n <= 0 && return 0
    fib = [2,1]; while fib[1] < n unshift!(fib,sum(fib[1:2])) end
    dig = Int[]; for f in fib f <= n ? (push!(dig,1); n = n-f;) : push!(dig,0) end
    return dig[1] == 0 ? dig[2:end] : dig
end
