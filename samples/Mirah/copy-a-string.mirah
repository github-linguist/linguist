src = "Hello"
new_alias = src

puts 'interned strings are equal' if src == new_alias

str_copy = String.new(src)
puts 'non-interned strings are not equal' if str_copy != src
puts 'compare strings with equals()' if str_copy.equals(src)
