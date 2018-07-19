filenames = { "f1.txt", "f2.txt" }

for _, fn in pairs( filenames ) do
    fp = io.open( fn, "r" )
    str = fp:read( "*all" )
    str = string.gsub( str, "Goodbye London!", "Hello New York!" )
    fp:close()

    fp = io.open( fn, "w+" )
    fp:write( str )
    fp:close()
end
