p "Enter a variable name"
x = gets.chomp!
instance_variable_set "@" + x, 42
p "The value of #{x} is #{instance_variable_get "@" + x}"
