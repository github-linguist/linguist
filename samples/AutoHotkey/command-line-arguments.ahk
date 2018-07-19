Loop %0% ; number of parameters
  params .= %A_Index% . A_Space
If params !=
  MsgBox, %0% parameters were passed:`n`n %params%
Else
  Run, %A_AhkPath% "%A_ScriptFullPath%" -c "\"alpha beta\"" -h "\"gamma\""
