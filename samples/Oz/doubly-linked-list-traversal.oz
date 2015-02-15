declare
  proc {Walk Node Action}
     case Node of nil then skip
     [] node(value:V next:N ...) then
	{Action V}
	{Walk @N Action}
     end
  end

  proc {WalkBackwards Node Action}
     Tail = {GetLast Node}
     proc {Loop N}
	case N of nil then skip
	[] node(value:V prev:P ...) then
	   {Action V}
	   {Loop @P}
	end
     end
  in
     {Loop Tail}
  end

  fun {GetLast Node}
     case @(Node.next) of nil then Node
     [] NextNode=node(...) then {GetLast NextNode}
     end
  end

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
  {Walk A Show}
  {WalkBackwards A Show}
