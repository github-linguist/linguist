; Known;
; A.
;         [ 151]
;        [a ][b ]
;      [40][c ][d ]
;    [e ][f ][g ][h ]
;  [ X][11][ Y][ 4][ Z]
;
; B.
;  Y = X + Z

Procedure.i SolveForZ(x)
  Protected a,b,c,d,e,f,g,h,z
  For z=0 To 20
    e=x+11: f=11+(x+z): g=(x+z)+4: h=4+z
    If e+f=40
      c=f+g : d=g+h: a=40+c: b=c+d
      If a+b=151
        ProcedureReturn z
      EndIf
    EndIf
  Next z
  ProcedureReturn -1
EndProcedure

Define x=-1, z=0, title$="Pascal's triangle/Puzzle in PureBasic"
Repeat
  x+1
  z=SolveForZ(x)
Until z>=0
MessageRequester(title$,"X="+Str(x)+#CRLF$+"Y="+Str(x+z)+#CRLF$+"Z="+Str(z))
