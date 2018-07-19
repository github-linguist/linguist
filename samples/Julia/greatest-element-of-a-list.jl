julia> maximum([1,3,3,7])
7

julia> maximum([pi,e+2/5,cos(6)/5,sqrt(91/10)])
3.141592653589793

julia> maximum([1,6,Inf])
Inf

julia> maximum(Float64[])
maximum: argument is empty
at In[138]:1
 in maximum at abstractarray.jl:1591
