def zigzag(n)
  indices = []
  n.times {|x| n.times {|y| indices << [x,y] }}
  zigzag = Array.new(n) {Array.new(n)}  # n x n array of nils
  indices.sort_by {|x,y| [x+y, (x+y).even? ? y : -y]} \
         .each_with_index {|(x,y),i| zigzag[x][y] = i}
  zigzag
end

def print_matrix(m)
  format = "%#{m.flatten.max.to_s.size}d "
  puts m.map {|row| row.map {|val| format % val}.join}
end

print_matrix zigzag(5)
