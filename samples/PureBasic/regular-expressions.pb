String$        = "<tag>some text consisting of Roman letters spaces and numbers like 12</tag>"
regex$         = "<([a-z]*)>[a-z,A-Z,0-9, ]*</\1>"
regex_replace$ = "letters[a-z,A-Z,0-9, ]*numbers[a-z,A-Z,0-9, ]*"
If CreateRegularExpression(1, regex$) And CreateRegularExpression(2, regex_replace$)
  If MatchRegularExpression(1, String$)
    Debug "Tags correct, and only alphanummeric or space characters between them"
  EndIf
  Debug ReplaceRegularExpression(2, String$, "char stuff")
EndIf
