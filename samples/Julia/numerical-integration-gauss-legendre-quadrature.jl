function gauss(a,b, N)
    λ, Q = eig(SymTridiagonal(zeros(N), [ n / sqrt(4n^2 - 1) for n = 1:N-1 ]))
    return (λ + 1) * (b-a)/2 + a, [ 2*Q[1,i]^2 for i = 1:N ] * (b-a)/2
end
