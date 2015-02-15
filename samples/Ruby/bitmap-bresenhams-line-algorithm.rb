Pixel = Struct.new(:x, :y)

class Pixmap

  def draw_line(p1, p2, colour)
    validate_pixel(p1.x, p2.y)
    validate_pixel(p2.x, p2.y)

    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y

    steep = (y2 - y1).abs > (x2 - x1).abs

    if steep
      x1, y1 = y1, x1
      x2, y2 = y2, x2
    end

    if x1 > x2
      x1, x2 = x2, x1
      y1, y2 = y2, y1
    end

    deltax = x2 - x1
    deltay = (y2 - y1).abs
    error = deltax / 2
    ystep = y1 < y2 ? 1 : -1

    y = y1
    x1.upto(x2) do |x|
      pixel = steep ? [y,x] : [x,y]
      self[*pixel] = colour
      error -= deltay
      if error < 0
        y += ystep
        error += deltax
      end
    end
  end
end

bitmap = Pixmap.new(500, 500)
bitmap.fill(RGBColour::BLUE)
10.step(430, 60) do |a|
  bitmap.draw_line(Pixel[10, 10], Pixel[490,a], RGBColour::YELLOW)
  bitmap.draw_line(Pixel[10, 10], Pixel[a,490], RGBColour::YELLOW)
end
bitmap.draw_line(Pixel[10, 10], Pixel[490,490], RGBColour::YELLOW)
