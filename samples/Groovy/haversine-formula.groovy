def haversine(lat1, lon1, lat2, lon2) {
  def R = 6372.8
  // In kilometers
  def dLat = Math.toRadians(lat2 - lat1)
  def dLon = Math.toRadians(lon2 - lon1)
  lat1 = Math.toRadians(lat1)
  lat2 = Math.toRadians(lat2)

  def a = Math.sin(dLat / 2) * Math.sin(dLat / 2) + Math.sin(dLon / 2) * Math.sin(dLon / 2) * Math.cos(lat1) * Math.cos(lat2)
  def c = 2 * Math.asin(Math.sqrt(a))
  R * c
}

haversine(36.12, -86.67, 33.94, -118.40)

> 2887.25995060711
