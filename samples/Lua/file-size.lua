function GetFileSize( filename )
    local fp = io.open( filename )
    if fp == nil then
 	return nil
    end
    local filesize = fp:seek( "end" )
    fp:close()
    return filesize
end
