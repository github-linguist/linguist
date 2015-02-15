input =
(
a
bb
ccc
ddd
ee
f
ggg
)
longestLen := 0, buffer := ""
Loop Parse, input, `n
{
   top := SubStr(buffer, 1, InStr(buffer, "`n"))
   StringReplace, top, top, `n
   If SubStr(A_LoopField, LongestLen) ; at least as long
      buffer .= A_LoopField "`n"
   If !SubStr(top, StrLen(A_LoopField)) ; longer
      buffer := A_LoopField "`n", LongestLen := StrLen(A_LoopField)
}
MsgBox % buffer
