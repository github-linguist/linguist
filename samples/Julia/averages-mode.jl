function modes(values)
    dict = Dict() # Values => Number of repetitions
    modesArray = typeof(values[1])[] # Array of the modes so far
    max = 0 # Max of repetitions so far

    for v in values
        # Add one to the dict[v] entry (create one if none)
        if v in keys(dict)
            dict[v] += 1
        else
            dict[v] = 1
        end

        # Update modesArray if the number of repetitions
        # of v reaches or surpasses the max value
        if dict[v] >= max
            if dict[v] > max
                empty!(modesArray)
                max += 1
            end
            append!(modesArray, [v])
        end
    end

    return modesArray
end

println(modes([1,3,6,6,6,6,7,7,12,12,17]))
println(modes((1,1,2,4,4)))
