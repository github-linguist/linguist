def partition(mask)
  return [[]] if mask.empty?
  sum = mask.inject(:+)
  a = Array.new(sum){|e| e}
  res = a.permutation.map do |perm|
    mask.map {|num_elts| perm.shift(num_elts).sort }
  end
  res.uniq
end

[[],[0,0,0],[1,1,1],[2,0,2]].each do |test_case|
  p test_case
  partition(test_case).each{|part| p part }
  puts
end
