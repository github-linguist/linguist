def identity(size)
  Array.new(size){|i| Array.new(size){|j| i==j ? 1 : 0}}
end

[4,5,6].each do |size|
  puts size, identity(size).map {|r| r.to_s}, ""
end
