Procedure.i ConsoleWrite(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        PrintN (t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure

Procedure.i StdOut(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        Print(t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure


DataSection
s:
Data.s "a"
Data.s "bb"
Data.s "ccc"
Data.s "ddd"
Data.s "ee"
Data.s "f"
Data.s "ggg"
Data.s "~"	; the tilda is only to keep the code compact
e:		; and easy to understand
EndDataSection

l$=""		; memory allocation for strings is automatic
a$=""		; in fact these two lines are unnecessary

Restore s

Repeat
Read.s s$
If s$="~":Break:EndIf
s$+#CRLF$
s=Len(s$):l=Len(l$)	; using s$ allows the use of s as an integer type
If     s>l		:l$=s$:a$=l$
ElseIf s=l		:a$+s$
EndIf
Forever

StdOut(a$)
