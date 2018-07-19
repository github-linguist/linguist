sSubject:= "greeting"
sText   := "hello"
sFrom   := "ahk@rosettacode"
sTo   := "whomitmayconcern"

sServer   := "smtp.gmail.com" ; specify your SMTP server
nPort     := 465 ; 25
bTLS      := True ; False
inputbox, sUsername, Username
inputbox, sPassword, password

COM_Init()
pmsg :=   COM_CreateObject("CDO.Message")
pcfg :=   COM_Invoke(pmsg, "Configuration")
pfld :=   COM_Invoke(pcfg, "Fields")

COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/sendusing", 2)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout", 60)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpserver", sServer)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpserverport", nPort)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpusessl", bTLS)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpauthenticate", 1)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/sendusername", sUsername)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/sendpassword", sPassword)
COM_Invoke(pfld, "Update")

COM_Invoke(pmsg, "Subject", sSubject)
COM_Invoke(pmsg, "From", sFrom)
COM_Invoke(pmsg, "To", sTo)
COM_Invoke(pmsg, "TextBody", sText)
COM_Invoke(pmsg, "Send")

COM_Release(pfld)
COM_Release(pcfg)
COM_Release(pmsg)
COM_Term()
#Include COM.ahk
