Pixel = Struct.new(:x, :y)

class Pixmap
  def draw_circle(pixel, radius, colour)
    validate_pixel(pixel.x, pixel.y)

    self[pixel.x, pixel.y + radius] = colour
    self[pixel.x, pixel.y - radius] = colour
    self[pixel.x + radius, pixel.y] = colour
    self[pixel.x - radius, pixel.y] = colour

    f = 1 - radius
    ddF_x = 1
    ddF_y = -2 * radius
    x = 0
    y = radius
    while x < y
      if f >= 0
        y -= 1
        ddF_y += 2
        f += ddF_y
      end
      x += 1
      ddF_x += 2
      f += ddF_x
      self[pixel.x + x, pixel.y + y] = colour
      self[pixel.x + x, pixel.y - y] = colour
      self[pixel.x - x, pixel.y + y] = colour
      self[pixel.x - x, pixel.y - y] = colour
      self[pixel.x + y, pixel.y + x] = colour
      self[pixel.x + y, pixel.y - x] = colour
      self[pixel.x - y, pixel.y + x] = colour
      self[pixel.x - y, pixel.y - x] = colour
    end
  end
end

bitmap = Pixmap.new(30, 30)
bitmap.draw_circle(Pixel[14,14], 12, RGBColour::BLACK)
