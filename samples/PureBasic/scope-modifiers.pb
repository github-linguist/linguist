;define a local integer variable by simply using it
baseAge.i = 10
;explicitly define local strings
Define person.s = "Amy", friend.s = "Susan"
;define variables that are both accessible inside and outside procedures
Global ageDiff = 3
Global extraYears = 5


Procedure test()
  ;define a local integer variable by simply using it
  baseAge.i = 30
  ;explicitly define a local string
  Define person.s = "Bob"
  ;allow access to a local variable in the main body of code
  Shared friend
  ;create a local variable distinct from a variable with global scope having the same name
  Protected extraYears = 2

  PrintN(person + " and " + friend + " are " + Str(baseAge) + " and " + Str(baseAge + ageDiff + extraYears) + " years old.")
EndProcedure


If OpenConsole()
  test()

  PrintN(person + " and " + friend + " are " + Str(baseAge) + " and " + Str(baseAge + ageDiff + extraYears) + " years old.")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
