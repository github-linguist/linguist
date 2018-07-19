a = [1,2,3]
b = [4,5,6]
ab = [a,b]
# the above bracket notation simply generates a call to vcat
ab = vcat(a,b)
# hcat is short for `horizontal concatenation`
ab = hcat(a,b) 	#ab -> 3x2 matrix
# the append!(a,b) method is mutating, appending `b` to `a`
append!(a,b)	# a now equals [1,2,3,4,5,6]
