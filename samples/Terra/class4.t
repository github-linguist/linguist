J = require("lib/javalike")

struct Shape {
	foo : int
}

Drawable = J.interface { draw = {} -> {} }

struct Square {
	length : int 
}
J.extends(Square,Shape)
J.implements(Square,Drawable)

terra Square:draw() : {}  end

terra bar()
 var a : &Square = Square.alloc()
 a:draw()
end

bar()