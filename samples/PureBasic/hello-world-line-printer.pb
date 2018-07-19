MyPrinter$ = LPRINT_GetDefaultPrinter()
If LPRINT_OpenPrinter(MyPrinter$)
  If LPRINT_StartDoc("Printing a RC-Task")
    LPRINT_Print(Chr(27) + "E") ; PCL reset for HP Printers
    LPRINT_PrintN("Hello World!")
    LPRINT_NewPage()
    LPRINT_EndDoc()
  EndIf
  LPRINT_ClosePrinter()
EndIf
