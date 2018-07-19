include Math
DtoR = PI/180

print 'Enter latitude: '
lat = Float( gets )
print 'Enter longitude: '
lng = Float( gets )
print 'Enter legal meridian: '
ref = Float( gets )
puts

slat = sin( lat * DtoR )

puts "    sine of latitude:  %.3f"% slat
puts "    diff longitude:    %.3f"% (lng-ref)
puts
puts 'Hour, sun hour angle, dial hour line angle from 6am to 6pm'
-6.upto(6) do |h|
  hra = 15 * h
  hra -= lng - ref
  hla =  atan( slat * tan( hra * DtoR ))/ DtoR
  puts "HR =%3d; HRA =%7.3f; HLA =%7.3f" % [h, hra, hla]
end
