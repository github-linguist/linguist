julia> a = [4,2,3,1]
4-element Int32 Array:
 4
 2
 3
 1
julia> sort(a) #out-of-place/non-mutating sort
4-element Int32 Array:
 1
 2
 3
 4

julia> a
4-element Int32 Array:
 4
 2
 3
 1

julia> sort!(a) # in-place/mutating sort
4-element Int32 Array:
 1
 2
 3
 4

julia> a
4-element Int32 Array:
 1
 2
 3
 4
