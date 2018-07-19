julia> @elapsed bigstr = string(BigInt(5)^4^3^2)
0.017507363

julia> length(bigstr)
183231

julia> bigstr[1:20]
"62060698786608744707"

julia> bigstr[end-20:end]
"892256259918212890625"
