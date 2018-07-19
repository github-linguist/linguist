Structure Language
  count.i
  Name.s
EndStructure

Dim Row.Language(2000)

; Lines has been split to fit RC's 80 char preferences
ignore$ = "Basic language learning Encyclopedia Implementations "
ignore$ + "Language Implementations Language users "
ignore$ + "Maintenance/OmitCategoriesCreated Programming Languages "
ignore$ + "Programming Tasks RCTemplates Solutions by Library Solutions by "
ignore$ + "Programming Language Solutions by Programming Task Unimplemented "
ignore$ + "tasks by language WikiStubs Examples needing attention "
ignore$ + "Impl needed"

URL$="http://www.rosettacode.org/w/index.php?"
URL$+"title=Special:Categories&limit=5000"

URLDownloadToFile_( #Null, URL$, "special.htm", 0, #Null)
ReadFile(0, "special.htm")
While Not Eof(0)
  i + 1
  x1$ =  ReadString(0)
  x2$ = Mid(x1$, FindString(x1$, "member", 1) - 4 , 3)
  Row(i)\count = Val(Trim(RemoveString(x2$, "(")))

  x3$ = Mid(x1$, FindString(x1$, Chr(34) + ">", 1) + 2, 30)
  Row(i)\Name = Left(x3$, FindString(x3$, "<", 1) - 1)
  If FindString(ignore$, Row(i)\Name, 1) Or Row(i)\Name = ""
    Row(i)\count = 0
  EndIf
Wend
offset=OffsetOf(Language\count)
SortStructuredArray(Row(), #PB_Sort_Descending, offset, #PB_Sort_Integer)
OpenConsole()
For i = 0 To 20
  PrintN( Str(i + 1) + ". " + Str(Row(i)\count) + " - " + Row(i)\Name)
Next
Input()
