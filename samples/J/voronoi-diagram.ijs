NB. (number of points) voronoi (shape)
NB. Generates an array of indices of the nearest point
voronoi =: 4 :0
  p =. (x,2) ?@$ y
  (i.<./)@:(+/@:*:@:-"1&p)"1 ,"0/&i./ y
)
viewmat 25 voronoi 500 500
