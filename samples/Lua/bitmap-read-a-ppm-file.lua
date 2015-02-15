function Read_PPM( filename )
    local fp = io.open( filename, "rb" )
    if fp == nil then return nil end

    local data = fp:read( "*line" )
    if data ~= "P6" then return nil end

    repeat
        data = fp:read( "*line" )
    until string.find( data, "#" ) == nil

    local image = {}
    local size_x, size_y

    size_x = string.match( data, "%d+" )
    size_y = string.match( data, "%s%d+" )

    data = fp:read( "*line" )
    if tonumber(data) ~= 255 then return nil end

    for i = 1, size_x do
        image[i] = {}
    end

    for j = 1, size_y do
        for i = 1, size_x do
            image[i][j] = { string.byte( fp:read(1) ), string.byte( fp:read(1) ), string.byte( fp:read(1) ) }
        end
    end

    fp:close()

    return image
end
