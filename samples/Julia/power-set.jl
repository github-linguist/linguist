function powerset (x)
  result = {{}}
  for i in x, j = 1:length(result)
    push!(result, [result[j],i])
  end
  result
end
