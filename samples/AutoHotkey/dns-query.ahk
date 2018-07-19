Url := "www.kame.net" , LogFile := "Ping_" A_Now ".log"
Runwait, %comspec% /c nslookup %Url%>%LogFile%, , hide
FileRead, Contents, %LogFile%
FileDelete, %LogFile%
RegExMatch(Contents,"Addresses:.+(`r?`n\s+.+)*",Match)
MsgBox, % RegExReplace(Match,"(Addresses:|[ `t])")
