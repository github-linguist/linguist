julia> findfirst(x -> x=="yes", {"no","?","yes","maybe","yes"})
3
julia> indexin({"yes"},{"no","?","yes","maybe","yes"})
1-element Array{Int64,1}:
 5
julia> findin({"no","?","yes","maybe","yes"},{"yes"})
2-element Array{Int64,1}:
 3
 5
julia> find(x -> x=="yes", {"no","?","yes","maybe","yes"})
2-element Array{Int64,1}:
 3
 5
