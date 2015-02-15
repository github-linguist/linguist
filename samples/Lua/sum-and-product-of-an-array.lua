function sumf(a, ...) return a and a + sumf(...) or 0 end
function sumt(t) return sumf(unpack(t)) end
function prodf(a, ...) return a and a * prodf(...) or 1 end
function prodt(t) return prodf(unpack(t)) end

print(sumt{1, 2, 3, 4, 5})
print(prodt{1, 2, 3, 4, 5})
