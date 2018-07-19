class T
  def name
    "T"
  end
end

class S
  def name
    "S"
  end
end

obj1 = T.new
obj2 = S.new
puts obj1.dup.name # prints "T"
puts obj2.dup.name # prints "S"
