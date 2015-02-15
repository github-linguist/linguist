#SymbolCount = 12 ;0 based count
DataSection
  denominations:
  Data.s "M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I" ;0-12

  denomValues:
  Data.i  1000,900,500,400,100,90,50,40,10,9,5,4,1 ;values in decending sequential order
EndDataSection

;-setup
Structure romanNumeral
  symbol.s
  value.i
EndStructure

Global Dim refRomanNum.romanNumeral(#SymbolCount)

Restore denominations
For i = 0 To #SymbolCount
  Read.s refRomanNum(i)\symbol
Next

Restore denomValues
For i = 0 To #SymbolCount
  Read refRomanNum(i)\value
Next

Procedure.s decRoman(n)
  ;converts a decimal number to a roman numeral
  Protected roman$, i

  For i = 0 To #SymbolCount
    Repeat
      If n >= refRomanNum(i)\value
        roman$ + refRomanNum(i)\symbol
        n - refRomanNum(i)\value
      Else
        Break
      EndIf
    ForEver
  Next

  ProcedureReturn roman$
EndProcedure

If OpenConsole()

  PrintN(decRoman(1999)) ;MCMXCIX
  PrintN(decRoman(1666)) ;MDCLXVI
  PrintN(decRoman(25))   ;XXV
  PrintN(decRoman(954))  ;CMLIV

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
