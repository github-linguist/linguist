julia> [1 2 3 ; 4 5 6] * [1 2 ; 3 4 ; 5 6]  # product of a 2x3 by a 3x2
2x2 Array{Int64,2}:
 22  28
 49  64

julia> [1 2 3] * [1,2,3]   # product of a row vector by a column vector
1-element Array{Int64,1}:
 14
