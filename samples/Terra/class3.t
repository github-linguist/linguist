
C = terralib.includec("stdio.h")

local Class = require("lib/javalike")


local Prints = Class.interface { print = {} -> {} }

struct Leaf {
  data : int
}
Class.implements(Leaf,Prints)

terra Leaf:print() : {} 
  C.printf("%d\n",self.data) 
end


struct Node {
  next : &Leaf
}
Class.extends(Node,Leaf)

terra Node:print() : {} 
  C.printf("%d\n",self.data) 
  self.next:print()
end

terra test()
  var a,b = Leaf.alloc(), Node.alloc()
  a.data,b.data,b.next = 1,2,a
  var p : &Prints = b
  p:print()
end

test()