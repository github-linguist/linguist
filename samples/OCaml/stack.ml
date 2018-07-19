exception Stack_empty

class ['a] stack =
  object (self)
    val mutable lst : 'a list = []

    method push x =
     lst <- x::lst

    method pop =
      match lst with
        []    -> raise Stack_empty
      | x::xs -> lst <- xs;
                 x

    method is_empty =
      lst = []
  end
