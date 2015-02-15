N = 10
D1 = speye(N-1,N) - spdiagm(ones(N-1),1,N-1,N)
D = [ kron(D1, speye(N)); kron(speye(N), D1) ]
i, j = N*1 + 2, N*7+7
b = zeros(N^2); b[i], b[j] = 1, -1
v = (D' * D) \ b
v[i] - v[j]
