class Pixmap
  def draw_bezier_curve(points, colour)
    # ensure the points are increasing along the x-axis
    points = points.sort_by {|p| [p.x, p.y]}
    xmin = points[0].x
    xmax = points[-1].x
    increment = 2
    prev = points[0]
    ((xmin + increment) .. xmax).step(increment) do |x|
      t = 1.0 * (x - xmin) / (xmax - xmin)
      p = Pixel[x, bezier(t, points).round]
      draw_line(prev, p, colour)
      prev = p
    end
  end
end

# the generalized n-degree Bezier summation
def bezier(t, points)
  n = points.length - 1
  points.each_with_index.inject(0.0) do |sum, (point, i)|
    sum += n.choose(i) * (1-t)**(n - i) * t**i * point.y
  end
end

class Fixnum
  def choose(k)
    self.factorial / (k.factorial * (self - k).factorial)
  end
  def factorial
    (2 .. self).reduce(1, :*)
  end
end

bitmap = Pixmap.new(400, 400)
points = [
  Pixel[40,100], Pixel[100,350], Pixel[150,50],
  Pixel[150,150], Pixel[350,250], Pixel[250,250]
]
points.each {|p| bitmap.draw_circle(p, 3, RGBColour::RED)}
bitmap.draw_bezier_curve(points, RGBColour::BLUE)
