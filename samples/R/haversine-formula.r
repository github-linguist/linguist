dms_to_rad <- function(d, m, s) (d + m / 60 + s / 3600) * pi / 180

# Volumetric mean radius is 6371 km, see http://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html
# The diameter is thus 12742 km

great_circle_distance <- function(lat1, long1, lat2, long2) {
   a <- sin(0.5 * (lat2 - lat1))
   b <- sin(0.5 * (long2 - long1))
   12742 * asin(sqrt(a * a + cos(lat1) * cos(lat2) * b * b))
}

# Coordinates are found here:
#     http://www.airport-data.com/airport/BNA/
#     http://www.airport-data.com/airport/LAX/

great_circle_distance(
   dms_to_rad(36,  7, 28.10), dms_to_rad( 86, 40, 41.50),   # Nashville International Airport (BNA)
   dms_to_rad(33, 56, 32.98), dms_to_rad(118, 24, 29.05))  # Los Angeles International Airport (LAX)

# Output:  2886.327
