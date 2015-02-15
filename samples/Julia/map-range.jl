maprange(s, a, b) = let a1 = minimum(a), a2 = maximum(a), b1 = minimum(b), b2 = maximum(b)
    b1 + (s-a1) * (b2-b1) / (a2-a1)
end
