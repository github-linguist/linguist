version = "1.0"
xmlheader := "<?xml version=" . version . "?>" . "<" . root . ">"

element("root", "child")
element("root", "element", "more text here")
element("root_child", "kid", "yak yak")
MsgBox % xmlheader . serialize("root")
Return

element(parent, name, text="")
{
  Global
  %parent%_children .= name . "`n"
  %parent%_%name% = %parent%_%name%
  %parent%_%name%_name := name
  %parent%_%name%_text := text
}

serialize(root){
  StringSplit, root, root, _
  xml .= "<" . root%root0% . ">"
  StringTrimRight, %root%_children, %root%_children, 1
  Loop, Parse, %root%_children, `n
  {
    If %root%_%A_LoopField%_children
      xml .= serialize(%root%_%A_LoopField%)
    Else
    {
      element := "<" . %root%_%A_LoopField%_name . ">"
      element .= %root%_%A_LoopField%_text
      element .= "</" . %root%_%A_LoopField%_name . ">"
      xml .= element
    }
  }
  Return xml .= "</" . root%root0% . ">"
}
