conf = {}

fp = io.open( "conf.txt", "r" )

for line in fp:lines() do
    line = line:match( "%s*(.+)" )
    if line and line:sub( 1, 1 ) ~= "#" and line:sub( 1, 1 ) ~= ";" then
 	option = line:match( "%S+" ):lower()
	value  = line:match( "%S*%s*(.*)" )

	if not value then
 	    conf[option] = true
	else
	    if not value:find( "," ) then
		conf[option] = value
	    else
		value = value .. ","
		conf[option] = {}
		for entry in value:gmatch( "%s*(.-)," ) do
		    conf[option][#conf[option]+1] = entry
		end
	    end
	end

    end
end

fp:close()


print( "fullname = ", conf["fullname"] )
print( "favouritefruit = ", conf["favouritefruit"] )
if conf["needspeeling"] then print( "needspeeling = true" ) else print( "needspeeling = false" ) end
if conf["seedsremoved"] then print( "seedsremoved = true" ) else print( "seedsremoved = false" ) end
if conf["otherfamily"] then
    print "otherfamily:"
    for _, entry in pairs( conf["otherfamily"] ) do
	print( "", entry )
    end
end
