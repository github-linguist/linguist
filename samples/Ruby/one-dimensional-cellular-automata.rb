def evolve(ary)
  ([0]+ary+[0]).each_cons(3).map{|a,b,c| a+b+c == 2 ? 1 : 0}
end

def printit(ary)
  puts ary.join.tr("01",".#")
end

ary = [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0]
printit ary
until ary == (new = evolve(ary))
  printit ary = new
end
