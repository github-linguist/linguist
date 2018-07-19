Structure point_f
  x.f
  y.f
EndStructure

Procedure isInside(*p.point_f, *cp1.point_f, *cp2.point_f)
  If (*cp2\x - *cp1\x) * (*p\y - *cp1\y) > (*cp2\y - *cp1\y) * (*p\x - *cp1\x)
    ProcedureReturn 1
  EndIf
EndProcedure

Procedure intersection(*cp1.point_f, *cp2.point_f, *s.point_f, *e.point_f, *newPoint.point_f)
  Protected.point_f dc, dp
  Protected.f n1, n2, n3
  dc\x = *cp1\x - *cp2\x: dc\y = *cp1\y - *cp2\y
  dp\x = *s\x - *e\x: dp\y = *s\y - *e\y
  n1 = *cp1\x * *cp2\y - *cp1\y * *cp2\x
  n2 = *s\x * *e\y - *s\y * *e\x
  n3 = 1 / (dc\x * dp\y - dc\y * dp\x)
  *newPoint\x = (n1 * dp\x - n2 * dc\x) * n3: *newPoint\y = (n1 * dp\y - n2 * dc\y) * n3
EndProcedure

Procedure clip(List vPolygon.point_f(), List vClippedBy.point_f(), List vClippedPolygon.point_f())
  Protected.point_f cp1, cp2, s, e, newPoint
  CopyList(vPolygon(), vClippedPolygon())
  If LastElement(vClippedBy())
    cp1 = vClippedBy()

    NewList vPreClipped.point_f()
    ForEach vClippedBy()
      cp2 = vClippedBy()
      CopyList(vClippedPolygon(), vPreClipped())
      ClearList(vClippedPolygon())
      If LastElement(vPreClipped())
        s = vPreClipped()
        ForEach vPreClipped()
          e = vPreClipped()
          If isInside(e, cp1, cp2)
            If Not isInside(s, cp1, cp2)
              intersection(cp1, cp2, s, e, newPoint)
              AddElement(vClippedPolygon()): vClippedPolygon() = newPoint
            EndIf
            AddElement(vClippedPolygon()): vClippedPolygon() = e
          ElseIf isInside(s, cp1, cp2)
            intersection(cp1, cp2, s, e, newPoint)
            AddElement(vClippedPolygon()): vClippedPolygon() = newPoint
          EndIf
          s = e
        Next
      EndIf
      cp1 = cp2
    Next
  EndIf
EndProcedure

DataSection
  Data.f 50,150, 200,50, 350,150, 350,300, 250,300, 200,250, 150,350, 100,250, 100,200 ;subjectPolygon's vertices (x,y)
  Data.f 100,100, 300,100, 300,300, 100,300 ;clipPolygon's vertices (x,y)
EndDataSection

NewList subjectPolygon.point_f()
For i = 1 To 9
  AddElement(subjectPolygon())
  Read.f subjectPolygon()\x
  Read.f subjectPolygon()\y
Next

NewList clipPolygon.point_f()
For i = 1 To 4
  AddElement(clipPolygon())
  Read.f clipPolygon()\x
  Read.f clipPolygon()\y
Next

NewList newPolygon.point_f()
clip(subjectPolygon(), clipPolygon(), newPolygon())
If OpenConsole()
  ForEach newPolygon()
    PrintN("(" + StrF(newPolygon()\x, 2) + ", " + StrF(newPolygon()\y, 2) + ")")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
