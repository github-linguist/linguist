main = print . traverse True $ create [10,20,30,40]

data DList a = Leaf | Node { prev::(DList a), elt::a, next::(DList a) }

create = go Leaf
    where go _    []     = Leaf
          go prev (x:xs) = current
              where current = Node prev x next
                    next    = go current xs

isLeaf Leaf    = True
isLeaf _       = False

lastNode Leaf  = Leaf
lastNode dl    = until (isLeaf.next) next dl

traverse _    Leaf            = []
traverse True (Node l v Leaf) = v : v : traverse False l
traverse dir  (Node l v r)    = v : traverse dir (if dir then r else l)
