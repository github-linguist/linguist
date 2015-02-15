function multiply(n, a, b) if a <= b then return n, multiply(n, a + 1, b) end end

a, b = io.read() + 0, io.read() + 0
matrix = {multiply({multiply(1, 1, b)}, 1, a)}
matrix[a][b] = 5
print(matrix[a][b])
print(matrix[1][1])
