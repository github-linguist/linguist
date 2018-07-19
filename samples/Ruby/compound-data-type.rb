Point = Struct.new(:x,:y)
pt = Point.new(6,7)
puts pt.x        #=> 6
pt.y = 3
puts pt          #=> #<struct Point x=6, y=3>

# The other way of accessing
pt = Point[2,3]
puts pt[:x]      #=> 2
pt['y'] = 5
puts pt          #=> #<struct Point x=2, y=5>

pt.each_pair{|member, value| puts "#{member} : #{value}"}
                 #=> x : 2
                 #=> y : 5
