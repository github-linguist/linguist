; usage: > fixtags.ahk input.txt ouput.txt
FileRead, text, %1%
langs = ada,awk,autohotkey,etc
slang = /lang
slang := "<" . slang . "/>"
Loop, Parse, langs, `,
{
  tag1 = <%A_LoopField%>
  tag2 = </%A_LoopField%>
  text := RegExReplace(text, tag1, "<lang " . A_LoopField . ">")
  text := RegExReplace(text, tag2, slang)
  text := RegExReplace(text, "<code (.+?)>(.*?)</code>"
          , "<lang $1>$2" . slang)
}
FileAppend, % text, %2%
