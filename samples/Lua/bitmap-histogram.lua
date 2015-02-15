function Histogram( image )
    local size_x, size_y = #image, #image[1]

    local histo = {}
    for i = 0, 255 do
        histo[i] = 0
    end

    for i = 1, size_x do
        for j = 1, size_y do
            histo[ image[i][j] ] = histo[ image[i][j] ] + 1
        end
    end

    return histo
end

function FindMedian( histogram )
    local sum_l, sum_r = 0, 0
    local left, right = 0, 255

    repeat
        if sum_l < sum_r then
            sum_l = sum_l + histogram[left]
            left = left + 1
        else
            sum_r = sum_r + histogram[right]
            right = right - 1
        end
    until left == right

    return left
end


bitmap = Read_PPM( "inputimage.ppm" )
gray_im = ConvertToGrayscaleImage( bitmap )
histogram = Histogram( gray_im )
median = FindMedian( histogram )

for i = 1, #gray_im do
    for j = 1, #gray_im[1] do
        if gray_im[i][j] < median then
            gray_im[i][j] = 0
        else
            gray_im[i][j] = 255
        end
    end
end

bitmap = ConvertToColorImage( gray_im )
Write_PPM( "outputimage.ppm", bitmap )
