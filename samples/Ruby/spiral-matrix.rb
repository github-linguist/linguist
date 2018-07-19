def spiral(n)
  spiral = Array.new(n) {Array.new(n, nil)}     # n x n array of nils
  runs = n.downto(0).each_cons(2).to_a.flatten  # n==5; [5,4,4,3,3,2,2,1,1,0]
  delta = [[1,0], [0,1], [-1,0], [0,-1]].cycle
  x, y, value = -1, 0, -1
  for run in runs
    dx, dy = delta.next
    run.times { spiral[y+=dy][x+=dx] = (value+=1) }
  end
  spiral
end

def print_matrix(m)
  max = m.flatten.map{|x| x.to_s.size}.max
  m.each {|row| puts row.map {|x| "%#{max}s " % x}.join}
end

print_matrix spiral(5)
