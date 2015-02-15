function squaresum(a, ...) return a and a^2 + squaresum(...) or 0 end
function squaresumt(t) return squaresum(unpack(t)) end

print(squaresumt{3, 5, 4, 1, 7})
