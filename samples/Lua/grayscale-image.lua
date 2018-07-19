function ConvertToGrayscaleImage( bitmap )
    local size_x, size_y = #bitmap, #bitmap[1]
    local gray_im = {}

    for i = 1, size_x do
        gray_im[i] = {}
        for j = 1, size_y do
            gray_im[i][j] = math.floor( 0.2126*bitmap[i][j][1] + 0.7152*bitmap[i][j][2] + 0.0722*bitmap[i][j][3] )
        end
    end

    return gray_im
end

function ConvertToColorImage( gray_im )
    local size_x, size_y = #gray_im, #gray_im[1]
    local bitmap = Allocate_Bitmap( size_x, size_y )         -- this function is defined at http://rosettacode.org/wiki/Basic_bitmap_storage#Lua

    for i = 1, size_x do
        for j = 1, size_y do
            bitmap[i][j] = { gray_im[i][j], gray_im[i][j], gray_im[i][j] }
        end
    end

    return bitmap
end
