: s>f s>d d>f ;
: deg>rad 174532925199433e-16 f* ;
: difference f- deg>rad 2 s>f f/ fsin fdup f* ;

: haversine                            ( lat1 lon1 lat2 lon2 -- haversine)
  frot difference                      ( lat1 lat2 dLon^2)
  frot frot fover fover                ( dLon^2 lat1 lat2 lat1 lat2)
  fswap difference                     ( dLon^2 lat1 lat2 dLat^2)
  fswap deg>rad fcos                   ( dLon^2 lat1 dLat^2 lat2)
  frot  deg>rad fcos f*                ( dLon^2 dLat2 lat1*lat2)
  frot  f* f+                          ( lat1*lat2*dLon^2+dLat^2)
  fsqrt fasin 127456 s>f f* 10 s>f f/  ( haversine)
;

36.12e -86.67e 33.94e -118.40e haversine cr f.
