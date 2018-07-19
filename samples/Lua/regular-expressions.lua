str1 = "This is a string!"
str2 = "string"

print( str1:match( str2 ) )
erg = str1:gsub( "a", "another" ); print( erg )
