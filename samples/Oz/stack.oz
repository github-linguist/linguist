functor
export
   New
   Push
   Pop
   Empty
define
   fun {New}
      {NewCell nil}
   end

   proc {Push Stack Element}
      NewStack
      %% Use atomic swap for thread safety
      OldStack = Stack := NewStack
   in
      NewStack = Element|OldStack
   end

   proc {Pop Stack ?Result}
      NewStack
      %% Use atomic swap for thread safety
      OldStack = Stack := NewStack
   in
      Result|NewStack = OldStack
   end

   fun {Empty Stack}
      @Stack == nil
   end
end
