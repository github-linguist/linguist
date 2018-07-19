local x = io.read()
local y = io.read()

print ("Sum: "       , (x + y))
print ("Difference: ", (x - y))
print ("Product: "   , (x * y))
print ("Quotient: "  , (x / y)) -- Does not truncate
print ("Remainder: " , (x % y)) -- Result has sign of right operand
print ("Exponent: "  , (x ^ y))
