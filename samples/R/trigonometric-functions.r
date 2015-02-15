deg <- function(radians) 180*radians/pi
rad <- function(degrees) degrees*pi/180
sind <- function(ang) sin(rad(ang))
cosd <- function(ang) cos(rad(ang))
tand <- function(ang) tan(rad(ang))
asind <- function(v) deg(asin(v))
acosd <- function(v) deg(acos(v))
atand <- function(v) deg(atan(v))

r <- pi/3
rd <- deg(r)

print( c( sin(r), sind(rd)) )
print( c( cos(r), cosd(rd)) )
print( c( tan(r), tand(rd)) )

S <- sin(pi/4)
C <- cos(pi/3)
T <- tan(pi/4)

print( c( asin(S), asind(S) ) )
print( c( acos(C), acosd(C) ) )
print( c( atan(T), atand(T) ) )
