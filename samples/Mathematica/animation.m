mystring = "Hello World! ";
Scroll[str_, dir_] := StringJoin @@ RotateLeft[str // Characters, dir];
GiveString[dir_] := (mystring = Scroll[mystring, dir]);
CreateDialog[{
   DynamicModule[{direction = -1},
    EventHandler[
     Dynamic[TextCell[
       Refresh[GiveString[direction], UpdateInterval -> 1/8]],
      TrackedSymbols -> {}], {"MouseClicked" :> (direction *= -1)}]]
   }];
