def sort_disjoint_sublist!(ar, indices)
  values = ar.values_at(*indices).sort
  indices.sort.zip(values).each{ |i,v| ar[i] = v }
  ar
end

values = [7, 6, 5, 4, 3, 2, 1, 0]
indices = [6, 1, 7]
p sort_disjoint_sublist!(values, indices)
