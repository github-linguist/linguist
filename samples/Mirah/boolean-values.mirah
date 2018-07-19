import java.util.ArrayList
import java.util.HashMap

# booleans
puts 'true is true' if true
puts 'false is false' if (!false)

# lists treated as booleans
x = ArrayList.new
puts "empty array is true" if x
x.add("an element")
puts "full array is true" if x
puts "isEmpty() is false" if !x.isEmpty()

# maps treated as booleans
map = HashMap.new
puts "empty map is true" if map
map.put('a', '1')
puts "full map is true" if map
puts "size() is 0 is false" if !(map.size() == 0)

# these things do not compile
# value = nil   # ==> cannot assign nil to Boolean value
# puts 'nil is false' if false == nil  # ==> cannot compare boolean to nil
# puts '0 is false' if (0 == false)    # ==> cannot compare int to false

#puts 'TRUE is true' if TRUE   # ==> TRUE does not exist
#puts 'FALSE is false' if !FALSE   # ==> FALSE does not exist
