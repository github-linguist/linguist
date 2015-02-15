require 'mathn'
require 'rubygems'
require 'gd2'
include GD2

def hough_transform(img)
  mx, my = img.w*0.5, img.h*0.5
  max_d = Math.sqrt(mx**2 + my**2)
  min_d = max_d * -1
  hough = Hash.new(0)
  (0..img.w).each do |x|
    puts "#{x} of #{img.w}"
    (0..img.h).each do |y|
      if img.pixel2color(img.get_pixel(x,y)).g > 32
        (0...180).each do |a|
          rad = a * (Math::PI / 180.0)
          d = (x-mx) * Math.cos(rad) + (y-my) * Math.sin(rad)
          hough["#{a.to_i}_#{d.to_i}"] = hough["#{a.to_i}_#{d.to_i}"] + 1
        end
      end
    end
  end
  heat = GD2::Image.import 'heatmap.png'
  out = GD2::Image::TrueColor.new(180,max_d*2)
  max = hough.values.max
  p max
  hough.each_pair do |k,v|
    a,d = k.split('_').map(&:to_i)
    c = (v / max) * 255
    c = heat.get_pixel(c,0)
    out.set_pixel(a, max_d + d, c)
  end
  out
end
