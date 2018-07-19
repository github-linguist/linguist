MsgBox, 4, Fork, Start another process?
IfMsgBox, Yes
    Run, %A_AhkPath% "%A_ScriptFullPath%"
MsgBox, 0, Fork, Stop this process.
