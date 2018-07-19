Structure wins
  stay.i
  redecide.i
EndStructure

#goat = 0
#car = 1
Procedure MontyHall(*results.wins)
  Dim Doors(2)
  Doors(Random(2)) = #car

  player = Random(2)
  Select Doors(player)
    Case #car
      *results\redecide + #goat
      *results\stay + #car
    Case #goat
      *results\redecide + #car
      *results\stay + #goat
  EndSelect
EndProcedure

OpenConsole()
#Tries = 1000000

Define results.wins

For i = 1 To #Tries
  MontyHall(@results)
Next

PrintN("Trial runs for each option: " + Str(#Tries))
PrintN("Wins when redeciding: " + Str(results\redecide) + " (" + StrD(results\redecide / #Tries * 100, 2) + "% chance)")
PrintN("Wins when sticking:   " + Str(results\stay) + " (" + StrD(results\stay / #Tries * 100, 2) + "% chance)")
Input()
