slurp(s)  = readcsv(IOBuffer(s*","))

conv(s)   = colon(map(int,(match(r"^(-?\d+)-(-?\d+)$", s).captures))...)

expand(s) = mapreduce(x -> isa(x,Number)? int(x): conv(x), vcat, slurp(s))
