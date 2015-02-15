str = " \t \r \n String with spaces  \t  \r  \n  "

print( string.format( "Leading whitespace removed: %s", str:match( "^%s*(.+)" ) ) )
print( string.format( "Trailing whitespace removed: %s", str:match( "(.-)%s*$" ) ) )
print( string.format( "Leading and trailing whitespace removed: %s", str:match( "^%s*(.-)%s*$" ) ) )
