gosub constants
names := xmlescape(names)
remarks := xmlescape(remarks)

stringsplit, remarks, remarks, `n
xml = "<CharacterRemarks>"

loop, parse, names, `n
  xml .= "<Character name=" . A_LoopField . ">" . remarks%A_Index%
  . "</Character>`n"

xml .= "</CharacterRemarks>"

msgbox % xml
return

xmlescape(string)
{
  static
  punc = ",>,<,<=,>=,',&  ; "
  xmlpunc = &quot;,&gt;,&lt;,&lt;=,&gt;=,&apos;,&amp;
  if !punc1
  {
	StringSplit, punc, punc, `,
	StringSplit, xmlpunc, xmlpunc, `,
  }
  escaped := string
  loop, parse, punc, `,
  {
  StringReplace, escaped, escaped, % A_LoopField, % xmlpunc%A_Index%, All
  }
  Return escaped
}

constants:
#LTrim
names =
(
  April
  Tam O'Shanter
  Emily
)

remarks =
(
  Bubbly: I'm > Tam and <= Emily
  Burns: "When chapman billies leave the street ..."
  Short & shrift
)
return
