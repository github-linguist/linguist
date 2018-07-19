if (A_AhkVersion < "1.0.48.03")
{
  MsgBox % "you are using" . A_AhkVersion . "`nplease upgrade to" . "1.0.48.03"
  ExitApp
}
bloop = -3
if bloop
  if IsFunc("abs")
    MsgBox % abs(bloop)
return
