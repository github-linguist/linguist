declare
  fun {CreateNewNode Value}
     node(prev:{NewCell nil}
          next:{NewCell nil}
          value:Value)
  end

  proc {InsertAfter Node NewNode}
     Next = Node.next
  in
     (NewNode.next) := @Next
     (NewNode.prev) := Node
     case @Next of nil then skip
     [] node(prev:NextPrev ...) then
        NextPrev := NewNode
     end
     Next := NewNode
  end

  A = {CreateNewNode a}
  B = {CreateNewNode b}
  C = {CreateNewNode c}
in
  {InsertAfter A B}
  {InsertAfter A C}
