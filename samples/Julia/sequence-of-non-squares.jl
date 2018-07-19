[n + ifloor(1/2 + sqrt(n)) for n = 1:22]

julia> intersect([1:1000].^2, [n + ifloor(1/2 + sqrt(n)) for n = 1:1000000])
0-element Array{Int64,1}
