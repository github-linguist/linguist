haystack = Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo
needle = bush, washington
Loop, Parse, needle, `,
{
  If InStr(haystack, A_LoopField)
    MsgBox, % A_LoopField
  Else
    MsgBox % A_LoopField . " not in haystack"
}
