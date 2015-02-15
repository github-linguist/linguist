import Text.Printf

-- The haversine of an angle.
hsin t = let u = sin (t/2) in u*u

-- The distance between two points, given by latitude and longtitude, on a
-- circle.  The points are specified in radians.
distRad radius (lat1, lng1) (lat2, lng2) =
  let hlat = hsin (lat2 - lat1)
      hlng = hsin (lng2 - lng1)
      root = sqrt (hlat + cos lat1 * cos lat2 * hlng)
  in 2 * radius * asin (min 1.0 root)

-- The distance between two points, given by latitude and longtitude, on a
-- circle.  The points are specified in degrees.
distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
  where deg2rad (t, u) = (d2r t, d2r u)
        d2r t = t * pi / 180

-- The approximate distance, in kilometers, between two points on Earth.  The
-- latitude and longtitude are assumed to be in degrees.
earthDist = distDeg 6372.8

main = do
  let bna = (36.12,  -86.67)
      lax = (33.94, -118.40)
      dst = earthDist bna lax :: Double
  printf "The distance between BNA and LAX is about %0.f km.\n" dst
