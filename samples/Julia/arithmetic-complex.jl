julia> z1 = 1.5 + 3im
julia> z2 = 1.5 + 1.5im
julia> z1 + z2
3.0 + 4.5im
julia> z1 - z2
0.0 + 1.5im
julia> z1 * z2
-2.25 + 6.75im
julia> z1 / z2
1.5 + 0.5im
julia> - z1
-1.5 - 3.0im
julia> conj(z1), z1'   # two ways to conjugate
(1.5 - 3.0im,1.5 - 3.0im)
julia> abs(z1)
3.3541019662496847
julia> z1^z2
-1.102482955327779 - 0.38306415117199305im
julia> real(z1)
1.5
julia> imag(z1)
3.0
