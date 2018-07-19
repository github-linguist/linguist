;An array, map, or list can be used as a parameter to a procedure and in the
;process contain values to be returned as well.
Procedure example_1(x, y, Array r(1))  ;array r() will contain the return values
  Dim r(2) ;clear and resize the array
  r(0) = x + y  ;return these values in the array
  r(1) = x - y
  r(2) = x * y
EndProcedure

;A pointer to memory or a structured variable may also be returned to reference
;multiple return values (requiring the memory to be manually freed afterwards).
Procedure example_2(x, y)
  Protected *result.POINT = AllocateMemory(SizeOf(POINT))
  *result\x = x
  *result\y = y

  ProcedureReturn *result ;*result points to a 'POINT' structure containing x and y
EndProcedure

If OpenConsole()
  Dim a(5)
  example_1(6, 5, a()) ;a() now contains {11, 1, 30}
  PrintN("Array returned with {" + Str(a(0)) + ", " + Str(a(1)) + ", " + Str(a(2)) + "}")

  Define *aPoint.POINT
  *aPoint = example_2(6, 5) ;*aPoint references structured memory containing {6, 5}

  PrintN("structured memory holds: (" + Str(*aPoint\x) + ", " + Str(*aPoint\y) + ")")
   FreeMemory(*aPoint) ;freememory

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
