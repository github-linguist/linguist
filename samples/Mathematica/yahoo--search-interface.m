Manipulate[
 Column[Flatten[
   StringCases[
    StringCases[
     URLFetch[
      "http://search.yahoo.com/search?p=" <> query <> "&b=" <>
       ToString@page], "<ol" ~~ ___ ~~ "</ol>"],
    "<a" ~~ Shortest[__] ~~ "class=\"yschttl spt\" href=\"" ~~
      Shortest[url__] ~~ "\"" ~~ Shortest[__] ~~ ">" ~~
      Shortest[title__] ~~
      "<div class=\"abstr\">" | "<div class=\"sm-abs\">" ~~
      Shortest[abstr__] ~~ "</div>" :>
     Column[{Hyperlink[Style[#[[1]], Larger], #[[2]]], #[[3]],
         Style[#[[2]], Smaller]} &@
       StringReplace[{title, url,
         abstr}, {"<" ~~ Shortest[__] ~~ ">" -> "",
         "&#" ~~ n : DigitCharacter ... ~~ ";" :>
          FromCharacterCode[FromDigits@n], "&amp;" -> "&",
         "&quot;" -> "\"", "&lt;" -> "<", "&gt;" -> ">"}]]], 1],
  Spacings -> 2], {{input, "", "Yahoo!"},
  InputField[Dynamic@input, String] &}, {{query, ""},
  ControlType -> None}, {{page, 1}, ControlType -> None},
 Row[{Button["Search", page = 1; query = input],
   Button["Prev", page -= 10, Enabled -> Dynamic[page >= 10]],
   Button["Next", page += 10]}]]
