from math import radians, sin, cos, sqrt, atan2

def haversine(lat1, lon1, lat2, lon2):

  R = 6372.8

# In kilometers

  dLat = radians(lat2 - lat1)
  dLon = radians(lon2 - lon1)
  lat1 = radians(lat1)
  lat2 = radians(lat2)

  a = sin(dLat/2.)*sin(dLat/2.) + sin(dLon/2.)*sin(dLon/2.)*cos(lat1)*cos(lat2)
  c = 2.*atan2(sqrt(a),sqrt(1-a))

  return R * c

>>> haversine(36.12, -86.67, 33.94, -118.40)
2887.2599506071106
>>>
