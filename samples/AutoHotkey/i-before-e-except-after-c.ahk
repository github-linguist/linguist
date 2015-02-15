WordList := URL_ToVar("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
WordList := RegExReplace(WordList, "i)cie", "", cieN)
WordList := RegExReplace(WordList, "i)cei", "", ceiN)
RegExReplace(WordList, "i)ie", "", ieN)
RegExReplace(WordList, "i)ei", "", eiN)

cei := ceiN / cieN > 2 ? "plausible" : "implausible"
ei  := ieN  / eiN  > 2 ? "plausible" : "implausible"
ova := cei = "plausible." && ei = "plausible" ? "plausible" : "implausible"

MsgBox, % """I before E when not preceded by C"" is " ei ".`n"
        . ieN " cases for and " eiN " cases against is a ratio of " ieN / eiN ".`n`n"
        . """E before I when preceded by C"" is " cei ".`n"
        . ceiN " cases for and " cieN " cases against is a ratio of " ceiN / cieN ".`n`n"
        . "Overall the rule is " ova "."

URL_ToVar(URL) {
    WebRequest := ComObjCreate("WinHttp.WinHttpRequest.5.1")
    WebRequest.Open("GET", URL)
    WebRequest.Send()
    return, WebRequest.ResponseText
}
