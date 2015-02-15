def ipart(n); n.truncate; end
def fpart(n); n - ipart(n); end
def rfpart(n); 1.0 - fpart(n); end

class Pixmap
  def draw_line_antialised(p1, p2, colour)
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
    gradient = 1.0 * deltay / deltax

    # handle the first endpoint
    xend = x1.round
    yend = y1 + gradient * (xend - x1)
    xgap = rfpart(x1 + 0.5)
    xpxl1 = xend
    ypxl1 = ipart(yend)
    put_colour(xpxl1, ypxl1, colour, steep, rfpart(yend)*xgap)
    put_colour(xpxl1, ypxl1 + 1, colour, steep, fpart(yend)*xgap)
    itery = yend + gradient

    # handle the second endpoint
    xend = x2.round
    yend = y2 + gradient * (xend - x2)
    xgap = rfpart(x2 + 0.5)
    xpxl2 = xend
    ypxl2 = ipart(yend)
    put_colour(xpxl2, ypxl2, colour, steep, rfpart(yend)*xgap)
    put_colour(xpxl2, ypxl2 + 1, colour, steep, fpart(yend)*xgap)

    # in between
    (xpxl1 + 1).upto(xpxl2 - 1).each do |x|
      put_colour(x, ipart(itery), colour, steep, rfpart(itery))
      put_colour(x, ipart(itery) + 1, colour, steep, fpart(itery))
      itery = itery + gradient
    end
  end

  def put_colour(x, y, colour, steep, c)
    x, y = y, x if steep
    self[x, y] = anti_alias(colour, self[x, y], c)
  end

  def anti_alias(new, old, ratio)
    blended = new.values.zip(old.values).map {|n, o| (n*ratio + o*(1.0 - ratio)).round}
    RGBColour.new(*blended)
  end
end

bitmap = Pixmap.new(500, 500)
bitmap.fill(RGBColour::BLUE)
10.step(430, 60) do |a|
  bitmap.draw_line_antialised(Pixel[10, 10], Pixel[490,a], RGBColour::YELLOW)
  bitmap.draw_line_antialised(Pixel[10, 10], Pixel[a,490], RGBColour::YELLOW)
end
bitmap.draw_line_antialised(Pixel[10, 10], Pixel[490,490], RGBColour::YELLOW)
