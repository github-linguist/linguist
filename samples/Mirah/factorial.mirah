def factorial_iterative(n:int)
    2.upto(n-1) do |i|
        n *= i
    end
    n
end

puts factorial_iterative 10
