def fibonacci(n:int)
    return n if n < 2
    fibPrev = 1
    fib = 1
    3.upto(Math.abs(n)) do
        oldFib = fib
        fib = fib + fibPrev
        fibPrev = oldFib
    end
    fib * (n<0 ? int(Math.pow(n+1, -1)) : 1)
end

puts fibonacci 1
puts fibonacci 2
puts fibonacci 3
puts fibonacci 4
puts fibonacci 5
puts fibonacci 6
puts fibonacci 7
