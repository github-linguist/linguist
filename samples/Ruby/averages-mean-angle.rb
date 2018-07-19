require 'complex' # Superfluous in Ruby >= 2.0; complex is added to core.

def deg2rad(d)
  d * Math::PI / 180
end

def rad2deg(r)
  r * 180 / Math::PI
end

def mean_angle(deg)
  rad2deg((deg.inject(0) {|z, d| z + Complex.polar(1, deg2rad(d))} / deg.length).arg)
end

[[350, 10], [90, 180, 270, 360], [10, 20, 30]].each {|angles|
  puts "The mean angle of %p is: %f degrees" % [angles, mean_angle(angles)]
}
