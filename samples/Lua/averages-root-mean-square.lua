function sumsq(a, ...) return a and a^2 + sumsq(...) or 0 end
function rms(t) return (sumsq(unpack(t)) / #t)^.5 end

print(rms{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
