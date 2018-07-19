Procedure.s booleanText(b) ;returns 'True' or 'False' for a boolean input
  If b: ProcedureReturn "True": EndIf
  ProcedureReturn "False"
EndProcedure

Procedure.s listSetElements(Map a(), delimeter.s = " ") ;format elements for display
  Protected output$

  ForEach a()
    output$ + MapKey(a()) + delimeter
  Next

  ProcedureReturn "(" + RTrim(output$, delimeter) + ")"
EndProcedure

Procedure.s listSortedSetElements(Map a(), delimeter.s = " ") ;format elements for display as sorted for easy comparison
  Protected output$
  NewList b.s()

  ForEach a()
    AddElement(b()): b() = MapKey(a())
  Next
  SortList(b(), #PB_Sort_Ascending | #PB_Sort_NoCase)
  ForEach b()
    output$ + b() + delimeter
  Next

  ProcedureReturn "(" + RTrim(output$, delimeter) + ")"
EndProcedure

Procedure cardinalityOf(Map a())
  ProcedureReturn MapSize(a())
EndProcedure

Procedure createSet(elements.s, Map o(), delimeter.s = " ", clearSet = 1)
  Protected i, elementCount

  If clearSet: ClearMap(o()): EndIf
  elementCount = CountString(elements, delimeter) + 1 ;add one for the last element which won't have a delimeter
  For i = 1 To elementCount
    AddMapElement(o(), StringField(elements, i, delimeter))
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure adjoinTo(elements.s, Map o(), delimeter.s = " ")
  ProcedureReturn createSet(elements, o(), delimeter, 0)
EndProcedure

Procedure disjoinFrom(elements.s, Map o(), delimeter.s = " ")
  Protected i, elementCount

  elementCount = CountString(elements, delimeter) + 1 ;add one for the last element which won't have a delimeter
  For i = 1 To elementCount
    DeleteMapElement(o(), StringField(elements, i, delimeter))
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure isElementOf(element.s, Map a())
  ProcedureReturn FindMapElement(a(), element)
EndProcedure



Procedure unionOf(Map a(), Map b(), Map o())
  CopyMap(a(), o())
  ForEach b()
    AddMapElement(o(), MapKey(b()))
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure intersectionOf(Map a(), Map b(), Map o())
  ClearMap(o())
  ForEach a()
    If FindMapElement(b(), MapKey(a()))
      AddMapElement(o(), MapKey(a()))
    EndIf
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure differenceOf(Map a(), Map b(), Map o())
  CopyMap(a(), o())
  ForEach b()
    If FindMapElement(o(), MapKey(b()))
      DeleteMapElement(o())
    Else
      AddMapElement(o(), MapKey(b()))
    EndIf
  Next

  ProcedureReturn MapSize(o())
EndProcedure

Procedure isSubsetOf(Map a(), Map b()) ;boolean
  ForEach a()
    If Not FindMapElement(b(), MapKey(a()))
      ProcedureReturn 0
    EndIf
  Next
  ProcedureReturn 1
EndProcedure

Procedure isProperSubsetOf(Map a(), Map b()) ;boolean
  If MapSize(a()) = MapSize(b())
    ProcedureReturn 0
  EndIf
  ProcedureReturn isSubsetOf(a(), b())
EndProcedure

Procedure isEqualTo(Map a(), Map b())
  If MapSize(a()) = MapSize(b())
    ProcedureReturn isSubsetOf(a(), b())
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure isEmpty(Map a()) ;boolean
  If MapSize(a())
    ProcedureReturn 0
  EndIf
  ProcedureReturn 1
EndProcedure

If OpenConsole()
  NewMap a()
  NewMap b()
  NewMap o() ;for output sets
  NewMap c()

  createSet("red blue green orange yellow", a())
  PrintN("Set A = " + listSortedSetElements(a()) + " of cardinality " + Str(cardinalityOf(a())) + ".")
  createSet("lady green red", b())
  PrintN("Set B = " + listSortedSetElements(b()) + " of cardinality " + Str(cardinalityOf(b())) + ".")
  PrintN("'red' is an element of A is " + booleanText(isElementOf("red", a())) + ".")
  PrintN("'red' is an element of B is " + booleanText(isElementOf("red", b())) + ".")
  PrintN("'blue' is an element of B is " + booleanText(isElementOf("blue", b())) + ".")

  unionOf(a(), b(), o())
  PrintN(#crlf$ + "Union of A & B is " + listSortedSetElements(o()) + ".")
  intersectionOf(a(), b(), o())
  PrintN("Intersection of  A & B is " + listSortedSetElements(o()) + ".")
  differenceOf(a(), b(), o())
  PrintN("Difference of  A & B is " + listSortedSetElements(o()) + ".")

  PrintN(listSortedSetElements(a()) + " equals " + listSortedSetElements(a()) + " is " + booleanText(isEqualTo(a(), a())) + ".")
  PrintN(listSortedSetElements(a()) + " equals " + listSortedSetElements(b()) + " is " + booleanText(isEqualTo(a(), b())) + ".")

  createSet("red green", c())
  PrintN(#crlf$ + listSortedSetElements(c()) + " is a subset of " + listSortedSetElements(a()) + " is "+ booleanText(isSubsetOf(c(), a())) + ".")
  PrintN(listSortedSetElements(c()) + " is a proper subset of " + listSortedSetElements(b()) + " is "+ booleanText(isProperSubsetOf(c(), b())) + ".")
  PrintN(listSortedSetElements(c()) + " is a proper subset of " + listSortedSetElements(a()) + " is "+ booleanText(isProperSubsetOf(c(), a())) + ".")
  PrintN(listSortedSetElements(b()) + " is a proper subset of " + listSortedSetElements(b()) + " is "+ booleanText(isProperSubsetOf(b(), b())) + ".")

  PrintN(#crlf$ + "Set C = " + listSortedSetElements(c()) + " of cardinality " + Str(cardinalityOf(c())) + ".")
  adjoinTo("dog cat mouse", c())
  PrintN("Add 'dog cat mouse' to C to get " + listSortedSetElements(c()) + " of cardinality " + Str(cardinalityOf(c())) + ".")
  disjoinFrom("red green dog", c())
  PrintN("Take away 'red green dog' from C to get " + listSortedSetElements(c()) + " of cardinality " + Str(cardinalityOf(c())) + ".")


  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
