# Lightweight JS objects (with CS sugar).
point =
  x: 5
  y: 3

console.log point.x, point.y # 5 3

# Heavier OO style
class Point
  constructor: (@x, @y) ->
  distance_from: (p2) ->
    dx = p2.x - @x
    dy = p2.y - @y
    Math.sqrt dx*dx + dy*dy

p1 = new Point(1, 6)
p2 = new Point(6, 18)
console.log p1 # { x: 1, y: 6 }
console.log p1.distance_from # [Function]
console.log p1.distance_from p2 # 13
