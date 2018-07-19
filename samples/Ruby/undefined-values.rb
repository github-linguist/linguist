# Check to see whether it is defined
puts "var is undefined at first check" unless defined? var

# Give it a value
var = "Chocolate"

# Check to see whether it is defined after we gave it the
# value "Chocolate"
puts "var is undefined at second check" unless defined? var

# I don't know any way of undefining a variable in Ruby

# Because most of the output is conditional, this serves as
# a clear indicator that the program has run to completion.
puts "Done"
