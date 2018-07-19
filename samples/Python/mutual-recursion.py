def F(n): return 1 if n == 0 else n - M(F(n-1))
def M(n): return 0 if n == 0 else n - F(M(n-1))

print ([ F(n) for n in range(20) ])
print ([ M(n) for n in range(20) ])
