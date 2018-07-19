StringCaseSense, On
Progress, b2 w120 zh0 fs9, Please wait ...
Sleep, 10

Link = http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000
FileDelete, Cats.html
URLDownloadToFile, %Link%, Cats.html
FileRead, Cats, Cats.html

Link1 = http://rosettacode.org/wiki/Category:Programming_Languages
FileDelete, lang1.htm
URLDownloadToFile, %Link1%, Lang1.htm
FileRead, Lang1, Lang1.htm

LookFor = (\(previous 200\) \(<a href=")(.+?)" title="Category:Programming Languages">next 200
RegExMatch(Lang1, LookFor, Link) ; Link2
StringReplace, Link2, Link2, &amp;, &

FileDelete, lang2.htm
URLDownloadToFile, http://www.rosettacode.org%Link2%, Lang2.htm
FileRead, Lang2, Lang2.htm
Languages := Lang1 Lang2

; create list of categories with member count
Loop, Parse, Cats, `n, `r
{
    If InStr(A_LoopField, "<li>") {
        LookFor = title=\"Category:(.+?)"
        RegExMatch(A_LoopField, LookFor, Name)
        RegExMatch(A_LoopField, "(\d*)\smembers", Count)
        CatsList .= Count1 "|" Name1 "`r`n"
    }
}

; create list of languages
RegExMatch(Languages, "(<h2>Subcategories</h2>)(.*)previous 200", Match)
LookFor = <a href="/wiki/Category:.*?" title="Category:.*?">(.*?)</a>(.*)
While RegExMatch(Match2, LookFor, Match)
    LangList .= Match1 "`r`n"

; create the final list
Loop, Parse, CatsList, `n, `r
{
    StringSplit, out, A_LoopField, |
    If RegExMatch(LangList, "m)^" out2 "$")
        FinalList .= A_LoopField "`r`n"
}

Sort, FinalList, RN
Gui, -MinimizeBox
Gui, Margin, 6
Gui, Add, ListView, y10 w363 r20 Grid, Rank|Members|Category
Loop, Parse, FinalList, `n, `r
{
    If A_LoopField {
        StringSplit, Item, A_LoopField, |
        LV_Add("", A_Index, Item1, Item2)
    }
}

LV_ModifyCol(1, "Integer")
LV_ModifyCol(2, "Integer")
LV_ModifyCol(3, 250)
FormatTime, Timestamp,, dd MMM yyyy
Progress, Off
Gui, Show,, Rosetta Categories - %Timestamp%
Return

GuiClose:
    ExitApp
Return
