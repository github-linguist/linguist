include Math

Radius = 6371  # rough radius of the Earth, in kilometers

def spherical_distance(start_coords, end_coords)
  lat1, long1 = deg2rad *start_coords
  lat2, long2 = deg2rad *end_coords
  2 * Radius * asin(sqrt(sin((lat2-lat1)/2)**2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)**2))
end

def deg2rad(lat, long)
  [lat * PI / 180, long * PI / 180]
end

bna = [36.12, -86.67]
lax = [33.94, -118.4]

puts "%.1f" % spherical_distance(bna, lax)
