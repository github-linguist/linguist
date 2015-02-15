filename = "Text1.txt"

fp = io.open( filename, "r" )
str = fp:read( "*all" )
fp:close()

stripped = string.gsub( str, "/%*.-%*/", "" )
print( stripped )
