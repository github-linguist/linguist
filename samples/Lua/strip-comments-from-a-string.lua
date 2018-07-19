comment_symbols = ";#"

s1 = "apples, pears # and bananas"
s2 = "apples, pears ; and bananas"

print ( string.match( s1, "[^"..comment_symbols.."]+" ) )
print ( string.match( s2, "[^"..comment_symbols.."]+" ) )
