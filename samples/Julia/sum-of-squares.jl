julia> sum([1,2,3,4,5].^2)
55

julia> sum([x^2 for x in [1,2,3,4,5]])
55

julia> mapreduce(x->x^2,+,[1:5])
55

julia> sum([x^2 for x in []])
0
