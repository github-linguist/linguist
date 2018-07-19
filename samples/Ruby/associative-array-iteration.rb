myDict = { "hello" => 13,
	   "world" => 31,
	   "!"     => 71 }

# iterating over key-value pairs:
myDict.each {|key, value| puts "key = #{key}, value = #{value}"}
# or
myDict.each_pair {|key, value| puts "key = #{key}, value = #{value}"}

# iterating over keys:
myDict.each_key {|key| puts "key = #{key}"}

# iterating over values:
myDict.each_value {|value| puts "value =#{value}"}
