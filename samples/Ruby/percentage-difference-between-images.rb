require 'raster_graphics'

class RGBColour
  # the difference between two colours
  def -(a_colour)
    (@red - a_colour.red).abs +
    (@green - a_colour.green).abs +
    (@blue - a_colour.blue).abs
  end
end

class Pixmap
  # the difference between two images
  def -(a_pixmap)
    if @width != a_pixmap.width or @height != a_pixmap.height
      raise ArgumentError, "can't compare images with different sizes"
    end
    sum = 0
    each_pixel {|x,y| sum += self[x,y] - a_pixmap[x,y]}
    Float(sum) / (@width * @height * 255 * 3)
  end
end

lenna50 = Pixmap.open_from_jpeg('Lenna50.jpg')
lenna100 = Pixmap.open_from_jpeg('Lenna100.jpg')

puts "difference: %.5f%%" % (100.0 * (lenna50 - lenna100))
