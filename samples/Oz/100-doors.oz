declare
  NumDoors = 100
  NumPasses = 100

  fun {NewDoor} closed end

  fun {Toggle Door}
     case Door of closed then open
     [] open then closed
     end
  end

  fun {Pass Doors I}
     {List.mapInd Doors
      fun {$ Index Door}
         if Index mod I == 0 then {Toggle Door}
         else Door
         end
      end}
  end

  Doors0 = {MakeList NumDoors}
  {ForAll Doors0 NewDoor}

  DoorsN = {FoldL {List.number 1 NumPasses 1} Pass Doors0}
in
  %% print open doors
  {List.forAllInd DoorsN
   proc {$ Index Door}
      if Door == open then
	 {System.showInfo "Door "#Index#" is open."}
      end
   end
  }
