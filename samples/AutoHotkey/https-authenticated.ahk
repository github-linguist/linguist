iWeb_Init()
pwb := iWeb_newGui(0, 0, 1000, 800)
iWeb_nav(pwb, "http://www.facebook.com/login.php?ref=pf")
iWeb_Term()
iWeb_complete(pwb)
inputbox, email, email
inputbox, pass, password
iWeb_setDomObj(pwb,"Email",email)
iWeb_setDomObj(pwb,"pass",pass)
iWeb_clickDomObj(pwb, "login")
return

#Include iweb.ahk
#Include COM.ahk
#Include COMinvokeDeep.ahk
