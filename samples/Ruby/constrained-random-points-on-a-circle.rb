points = (1...100).map {
    # choose a random radius and angle
    angle = rand * 2.0 * Math::PI
    rad   = rand * 5.0 + 10.0
    # convert back from polar to cartesian coordinates
    [rad * Math::cos(angle), rad * Math::sin(angle)].map(&:round)
  }

(-15..15).each do |row|
  puts((-15..15).map { |col| points.include?([row, col]) ? "X" : " " }.join)
end

load 'raster_graphics.rb'

pixmap = Pixmap.new(321,321)
pixmap.draw_circle(Pixel.new(160,160),90,RGBColour::BLACK)
pixmap.draw_circle(Pixel.new(160,160),160,RGBColour::BLACK)
points.each {|(x,y)| pixmap[10*(x+16),10*(y+16)] = RGBColour::BLACK}
pngfile = __FILE__
pngfile[/\.rb/] = ".png"
pixmap.save_as_png(pngfile)
