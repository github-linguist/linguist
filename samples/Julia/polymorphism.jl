import Base.print

type Point
	x::Float64
	y::Float64
end

print(p::Point) = println("Point($(p.x), $(p.y))")

x(p::Point) = p.x
y(p::Point) = p.y

setx(p::Point, x) = (p.x = x)
sety(p::Point, y) = (p.y = y)

type Circle
	x::Float64
	y::Float64
	r::Float64
end

x(c::Circle) = c.x
y(c::Circle) = c.y
r(c::Circle) = c.r

setx(c::Circle, x) = (c.x = x)
sety(c::Circle, y) = (c.y = y)
setr(c::Circle, r) = (c.r = r)

print(c::Circle) = println("Circle($(c.x), $(c.y), $(c.r))")
