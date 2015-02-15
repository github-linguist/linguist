cube = proc{|x| x ** 3}
croot = proc{|x| x ** (1.quo 3)}
compose = proc {|f,g| proc {|x| f[g[x]]}}
funclist = [Math.method(:sin), Math.method(:cos), cube]
invlist = [Math.method(:asin), Math.method(:acos), croot]

puts funclist.zip(invlist).map {|f, invf| compose[invf, f][0.5]}
