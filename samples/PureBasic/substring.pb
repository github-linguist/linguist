If OpenConsole()

  Define baseString.s, m, n

  baseString = "Thequickbrownfoxjumpsoverthelazydog."
  n = 12
  m = 5

  ;Display the substring starting from n characters in and of m length.
  PrintN(Mid(baseString, n, m))

  ;Display the substring starting from n characters in, up to the end of the string.
  PrintN(Mid(baseString, n)) ;or PrintN(Right(baseString, Len(baseString) - n))

  ;Display the substring whole string minus last character
  PrintN(Left(baseString, Len(baseString) - 1))

  ;Display the substring starting from a known character within the string and of m length.
  PrintN(Mid(baseString, FindString(baseString, "b", 1), m))

  ;Display the substring starting from a known substring within the string and of m length.
  PrintN(Mid(baseString, FindString(baseString, "ju", 1), m))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
