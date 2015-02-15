#BaseTime =50
#Frequence=1250
#Short    =   #BaseTime
#Long     =3* #BaseTime
#Intergap =   #BaseTime
#LetterGap=3* #BaseTime
#WordGap  =7* #BaseTime

Declare.s TextToMorse(Text$)
Declare.i PlayMorse(Text$)

Text$ =InputRequester("Morse coder","Enter text to send","Hello RosettaWorld!")
Text$ =TextToMorse(Text$)
If Not (InitSound() And PlayMorse(Text$))
  Text$=ReplaceString(Text$, ",","")
  MessageRequester("Morse EnCoded",Text$)
EndIf

;-
Procedure PlayMorse(Code$)    ;- Beep() is normally only Ok on Windows_x86
  CompilerIf #PB_Compiler_Processor=#PB_Processor_x86 And #PB_Compiler_OS=#PB_OS_Windows
    Protected i, sign
    For i=1 To Len(Code$)
      sign=Asc(Mid(Code$,i,1))
      Select sign
        Case '.': Beep_(#Frequence,#Short): Delay(#Intergap)
        Case '-': Beep_(#Frequence,#Long) : Delay(#Intergap)
        Case ',': Delay(#LetterGap)
        Case ' ': Delay(#WordGap)
      EndSelect
    Next
    ProcedureReturn 1
  CompilerElse
    ProcedureReturn 0
  CompilerEndIf
EndProcedure

Procedure.s TextToMorse(InString$)
  Protected *p.Character=@InString$, CurrStr$, i=1
  Protected.s s1, s2, result
  Repeat
    If Not *p\c: Break: EndIf
    CurrStr$=UCase(PeekS(*p,1))
    *p+StringByteLength(">")
    Restore MorseCode
    Repeat
      Read.s s1
      If s1="Done"
        s2+s1+" " ; failed to find this coding
        Break
      ElseIf Not s1=CurrStr$
        Continue
      EndIf
      Read.s s2
      result+s2
      If s2<>" "
        result+","
      EndIf
    ForEver
  ForEver
  ProcedureReturn result
EndProcedure

DataSection
  MorseCode:
  Data.s "A",  ".-"
  Data.s "B",  "-..."
  Data.s "C",  "-.-."
  Data.s "D",  "-.."
  Data.s "E",  "."
  Data.s "F",  "..-."
  Data.s "G",  "--."
  Data.s "H",  "...."
  Data.s "I",  ".."
  Data.s "J",  ".---"
  Data.s "K",  "-.-"
  Data.s "L",  ".-.."
  Data.s "M",  "--"
  Data.s "N",  "-."
  Data.s "O",  "---"
  Data.s "P",  ".--."
  Data.s "Q",  "--.-"
  Data.s "R",  ".-."
  Data.s "S",  "..."
  Data.s "T",  "-"
  Data.s "U",  "..-"
  Data.s "V",  "...-"
  Data.s "W",  ".--"
  Data.s "X",  "-..-"
  Data.s "Y",  "-.--"
  Data.s "Z",  "--.."
  Data.s "Á",  "--.-"
  Data.s "Ä",  ".-.-"
  Data.s "É",  "..-.."
  Data.s "Ñ",  "--.--"
  Data.s "Ö",  "---."
  Data.s "Ü",  "..--"
  Data.s "1",  ".----"
  Data.s "2",  "..---"
  Data.s "3",  "...--"
  Data.s "4",  "....-"
  Data.s "5",  "....."
  Data.s "6",  "-...."
  Data.s "7",  "--..."
  Data.s "8",  "---.."
  Data.s "9",  "----."
  Data.s "0",  "-----"
  Data.s ",",  "--..--"
  Data.s ".",  ".-.-.-"
  Data.s "?",  "..--.."
  Data.s ";",  "-.-.-"
  Data.s ":",  "---..."
  Data.s "/",  "-..-."
  Data.s "-",  "-....-"
  Data.s "'",  ".----."
  Data.s "+",  ".-.-."
  Data.s "-",  "-....-"
  Data.s #DOUBLEQUOTE$, ".-..-."
  Data.s "@",  ".--.-."
  Data.s "(",  "-.--."
  Data.s ")",  "-.--.-"
  Data.s "_",  "..--.-"
  Data.s "$",  "...-..-"
  Data.s "&",  ".-..."
  Data.s "=",  "---..."
  Data.s " ",  " "
  Data.s  "Done",""
  EndOfMorseCode:
EndDataSection
