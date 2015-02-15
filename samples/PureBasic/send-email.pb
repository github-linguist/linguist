InitNetwork()

CreateMail(0, "from@mydomain.com", "This is the Subject")

SetMailBody(0, "Hello   " + Chr(10) + "This is a mail !")

AddMailRecipient(0, "test@yourdomain.com", #PB_Mail_To)

AddMailRecipient(0, "test2@yourdomain.com", #PB_Mail_Cc)

If SendMail(0, "smtp.mail.com")
    MessageRequester("Information", "Mail correctly sent !")
Else
    MessageRequester("Error", "Can't sent the mail !")
EndIf
