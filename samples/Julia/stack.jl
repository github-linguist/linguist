stack = {}
push!(stack, 1)
push!(stack, 2)
push!(stack, 3)
println(pop!(stack)) # 3
println(length(stack)) # 2
empty!(stack)
println(length(stack)) # 0
