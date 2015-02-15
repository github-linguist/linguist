def sierpinski_carpet(n)
  carpet = ["#"]
  n.times do
    carpet = carpet.collect {|x| x + x + x} + \
             carpet.collect {|x| x + x.tr("#"," ") + x} + \
             carpet.collect {|x| x + x + x}
  end
  carpet.join("\n")
end

puts sierpinski_carpet(3)
