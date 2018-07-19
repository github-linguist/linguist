def bind_x_to_value(x)
  binding
end

def eval_with_x(code, a, b)
  eval(code, bind_x_to_value(b)) - eval(code, bind_x_to_value(a))
end

puts eval_with_x('2 ** x', 3, 5) # Prints "24"
