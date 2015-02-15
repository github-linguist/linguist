; This is a small demo-code to demonstrate PureBasic’s internal
; documentation system.

;- All Includes
; By starting the line with ‘;-‘ marks that specific line as a special comment,
; and this will be included in the overview, while normal comments will not.

IncludeFile "MyLibs.pbi"
IncludeFile "Combustion_Data.pbi"

;-
;- Start of functions and Macros
;- Engeneering stuff

; A small function to calculate gas properties
Procedure.f CalcR( p.f, V.f, T.f)
  ProcedureReturn p*V/T
EndProcedure

; Example of a Macro
; These are indicated by '+' in the overview
Macro HalfPI()
  (#PI/2)
EndMacro

;-
;- - - - - - - - - - -
;- IO-Functions

Procedure Write_and_Close( File, Text$)
  If IsFile(File)
    WriteString(File,Text$)
    CloseFile(file)
  EndIf
EndProcedure
