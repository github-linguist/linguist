def main
  n = (ARGV[0] || 41).to_i
  k = (ARGV[1] || 3).to_i
  puts josephus(n,k)
end

def josephus(n, k)
  prisoners = (0...n).to_a
  prisoners.rotate!(k-1).shift  while prisoners.length > 1
  return prisoners.first
end

main
