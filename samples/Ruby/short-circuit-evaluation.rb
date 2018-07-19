def a( bool )
  puts "a( #{bool} ) called"
  bool
end

def b( bool )
  puts "b( #{bool} ) called"
  bool
end

 [true, false].each do |a_val|
   [true, false].each do |b_val|
     puts "a( #{a_val} ) and b( #{b_val} ) is #{a( a_val ) and b( b_val )}."
     puts
     puts "a( #{a_val} ) or b( #{b_val} ) is #{a( a_val)  or b( b_val )}."
     puts
   end
 end
