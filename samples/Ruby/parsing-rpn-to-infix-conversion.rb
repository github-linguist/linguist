rpn = RPNExpression.new("3 4 2 * 1 5 - 2 3 ^ ^ / +")
infix = rpn.to_infix
ruby = rpn.to_ruby
