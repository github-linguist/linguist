require 'objspace'

p ObjectSpace.memsize_of("a"*23)    #=> 0
p ObjectSpace.memsize_of("a"*24)    #=> 25
p ObjectSpace.memsize_of("a"*1000) #=> 1001
