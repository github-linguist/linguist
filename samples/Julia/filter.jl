julia> filter(iseven, [1,2,3,4,5,6,7,8,9])
4-element Array{Int64,1}:
 2
 4
 6
 8

julia> filter(isdigit, "distance: 150 000 000")
"150000000"
