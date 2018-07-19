Structure item
  name.s
  weight.f   ;units are kilograms (kg)
  Value.f
  vDensity.f ;the density of the value, i.e. value/weight, and yes I made up the term ;)
EndStructure

#maxWeight = 15
Global itemCount = 0 ;this will be increased as needed to match actual count
Global Dim items.item(itemCount)

Procedure addItem(name.s, weight.f, Value.f)
  If itemCount >= ArraySize(items())
    Redim items.item(itemCount + 10)
  EndIf
  With items(itemCount)
    \name = name
    \weight = weight
    \Value = Value
    If Not \weight
      \vDensity = \Value
    Else
      \vDensity = \Value / \weight
    EndIf
  EndWith
  itemCount + 1
EndProcedure

;build item list
addItem("beef", 3.8, 36)
addItem("pork", 5.4, 43)
addItem("ham", 3.6, 90)
addItem("greaves", 2.4, 45)
addItem("flitch", 4.0, 30)
addItem("brawn", 2.5, 56)
addItem("welt", 3.7, 67)
addItem("salami", 3.0, 95)
addItem("sausage", 5.9, 98)
SortStructuredArray(items(), #PB_Sort_descending, OffsetOf(item\vDensity), #PB_Sort_Float, 0, itemCount - 1)

Define TotalWeight.f, TotalValue.f, i
NewList knapsack.item()
For i = 0 To itemCount
  If TotalWeight + items(i)\weight < #maxWeight
    AddElement(knapsack())
    knapsack() = items(i)
    TotalWeight + items(i)\weight
    TotalValue + items(i)\Value
  Else
    AddElement(knapsack())
    knapsack() = items(i)
    knapsack()\weight = #maxWeight - TotalWeight
    knapsack()\Value = knapsack()\weight * knapsack()\vDensity
    TotalWeight = #maxWeight
    TotalValue + knapsack()\Value
    Break
  EndIf
Next

If OpenConsole()
  PrintN(LSet("Maximal weight", 26, " ") + "= " + Str(#maxWeight) + " kg")
  PrintN(LSet("Total weight of solution", 26, " ") + "= " + Str(#maxWeight) + " kg")
  PrintN(LSet("Total value", 26, " ") + "= " + StrF(TotalValue, 3) + " " + #CRLF$)
  PrintN("You can carry the following materials in the knapsack: ")
  ForEach knapsack()
    PrintN(RSet(StrF(knapsack()\weight, 1), 5, " ") + " kg  " + LSet(knapsack()\name, 10, " ") + "  (Value = " + StrF(knapsack()\Value, 3) + ")")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
