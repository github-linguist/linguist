URL      := "https://sourceforge.net/"
WININET_Init()
msgbox % html := UrlGetContents(URL)
WININET_UnInit()
return
#include urlgetcontents.ahk
#include wininet.ahk
