Point = Struct.new(:x, :y)
Line = Struct.new(:start, :stop)

Shoes.app(:width => 800, :height => 600, :resizable => false) do

  def split_segments(n)
    dir = 1
    @segments = @segments.inject([]) do |new, l|
      a, b, c, d = l.start.x, l.start.y, l.stop.x, l.stop.y

      mid_x = a + (c-a)/2.0 - (d-b)/2.0*dir
      mid_y = b + (d-b)/2.0 + (c-a)/2.0*dir
      mid_p = Point.new(mid_x, mid_y)

      dir *= -1
      new << Line.new(l.start, mid_p)
      new << Line.new(mid_p, l.stop)
    end
  end

  @segments = [Line.new(Point.new(200,200), Point.new(600,200))]
  15.times do |n|
    info "calculating frame #{n}"
    split_segments(n)
  end

  stack do
    @segments.each do |l|
      line l.start.x, l.start.y, l.stop.x, l.stop.y
    end
  end
end
