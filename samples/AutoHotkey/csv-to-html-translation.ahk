CSVData =
(
Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!
)
TableData := "<table>"
Loop Parse, CSVData,`n
{
   TableData .= "`n  <tr>"
   Loop Parse, A_LoopField, CSV
      TableData .= "<td>" HTMLEncode(A_LoopField) "</td>"
   TableData .= "</tr>"
}
TableData .= "`n</table>"
HTMLEncode(str){
   static rep := "&amp;<lt;>gt;""quot"
   Loop Parse, rep,;
      StringReplace, str, str, % SubStr(A_LoopField, 1, 1), % "&" . SubStr(A_LoopField, 2) . ";", All
   return str
}
MsgBox % clipboard := TableData
