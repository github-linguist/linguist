NewList MyList()  ; To hold a unknown amount of numbers to calculate

If OpenConsole()
  Define.d result
  Define i, sum_of_squares

  ;Populate a random amounts of numbers to calculate
  For i=0 To (Random(45)+5) ; max elements is unknown to the program
    AddElement(MyList())
    MyList()=Random(15)  ; Put in a random number
  Next

  Print("Averages/Root mean square"+#CRLF$+"of : ")

  ; Calculate square of each element, print each & add them together
  ForEach MyList()
    Print(Str(MyList())+" ")             ; Present to our user
    sum_of_squares+MyList()*MyList()     ; Sum the squares, e.g
  Next

  ;Present the result
  result=Sqr(sum_of_squares/ListSize(MyList()))
  PrintN(#CRLF$+"= "+StrD(result))

  PrintN("Press ENTER to exit"): Input()
  CloseConsole()
EndIf
