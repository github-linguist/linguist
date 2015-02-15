s1 = "string"
s2 = "str"
s3 = "ing"
s4 = "xyz"

print( "s1 starts with s2: ", string.find( s1, s2 ) == 1 )
print( "s1 starts with s3: ", string.find( s1, s3 ) == 1, "\n" )

print( "s1 contains s3: ", string.find( s1, s3 ) ~= nil )
print( "s1 contains s3: ", string.find( s1, s4 ) ~= nil, "\n" )

print( "s1 ends with s2: ", select( 2, string.find( s1, s2 ) ) == string.len( s1 ) )
print( "s1 ends with s3: ", select( 2, string.find( s1, s3 ) ) == string.len( s1 ) )
