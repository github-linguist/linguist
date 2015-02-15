#NoEnv ; do not resolve environment variables (speed)
#SingleInstance force ; allow only one instance
SetBatchLines, -1 ; set to highest script speed
SetControlDelay, 0 ; set delay between control commands to lowest

; additional label, for clarity only
AutoExec:
  Gui, Add, DDL, x10 y10 w270 r20 vlngStr ; add drop-down list
  Gui, Add, Button, x290 y10 gShowLng, Show Unimplemented ; add button
  Gui, Add, ListView, x10 y36 w400 h300 vcatLst gOnLV, Unimplemented ; add listview
  Gui, Add, StatusBar, vsbStr, ; add statusbar
  Gui, Show, , RosettaCode unimplemented list ; show the gui
  allLng := getList("lng") ; get a list of all available languages
  selectStr = Select language... ; set selection string for ddl
  GuiControl, , LngStr, |%selectStr%||%allLng% ; set the ddl to new contents
  SB_SetIcon("user32.dll", 5) ; change statusbar icon
  SB_SetText("Done loading languages. Select from menu.") ; change statusbar text
Return ; end of the autoexec section. Script waits for input.

; subroutine for language list in ddl update
ShowLng:
  Gui, Submit, NoHide ; refresh all gui variables
  If (lngStr != selectStr) ; if the current selected language is not the selection string
  {
    SB_SetIcon("user32.dll", 2) ; set statusbar icon to exclamation
    SB_SetText("Loading unimplemented tasks... Please wait.") ; set statusbar text
    allTsk := getList("tsk") ; get a list of all tasks
    allThis := getList(lngStr) ; get a list of all done tasks for this language
    Loop, Parse, allTsk, | ; parse the list of all tasks
    {
      If !InStr(allThis,A_LoopField) ; if the current task is not found in the list of all tasks
        notImpl .= A_LoopField . "|" ; add the current field to the notImpl variable
      allTskCnt := A_Index ; save the index for final count
    }
    StringTrimRight, notImpl, notImpl, 1 ; remove last delimiter
    LV_Delete() ; emty the listview
    GuiControl, -Redraw, catLst ; set the listview to not redraw while adding (speed)
    Loop, Parse, notImpl, | ; parse the list of not implemented tasks
    {
      LV_Add("", A_LoopField) ; add them to the listview, field by field
      notImplCnt := A_Index ; save the index for final count
    }
    GuiControl, +Redraw, catLst ; set the listview back to normal
    SB_SetIcon("user32.dll", 5) ; change statusbar icon to information
    SB_SetText("There are " . notImplCnt . " of " . allTskCnt
      . " tasks unimplemented. Double-click task to open in browser.") ; set the statusbar text
    notImpl = ; empty the notImpl variable
    notImplCnt = 0 ; set the count back to 0
  }
Return

; subroutine for action on listview
OnLV:
  If (A_GuiEvent = "DoubleClick") ; if the listview was double-clicked
  {
    LV_GetText(rowTxt, A_EventInfo) ; get the text of the clicked row
    rowTxt := Enc_Uri(rowTxt) ; uri-encode the text
    StringReplace, rowTxt, rowTxt, `%20, _, All ; replace all space-encodings with underscores
    Run % "http://rosettacode.org/wiki/" . rowTxt ; run the resulting link in the default browser
  }
Return

; generic function to gather list
getList(category)
{
  fileName = temp.xml ; set temp-file name
  If (category = "lng") ; if the category received is lng
  {
    category = Programming_Languages ; set the category
    fileName = lng.xml ; set temp-file name
    IfExist, %fileName% ; if the file already exists
    {
      FileGetTime, modTime, %fileName% ; get the last modified time
      EnvSub, modTime, %A_Now%, days ; get the difference between now and last modified time in days
      If (modTime < 3) ; if the result is less than 3
        Goto, GetFileNoDL ; go to function-internal subroutine to parse
    }
    SB_SetIcon("user32.dll", 2) ; set statusbar icon
    SB_SetText("Loading languages... Please wait.") ; set statusbar text
  }
  If (category = "tsk") ; if the category received is tsk
    category = Programming_Tasks ; set the category

  getFile: ; function-internal subroutine for getting a file and parsing it
    ; construct the url
    url := "http://www.rosettacode.org/w/api.php?action=query&list="
          . "categorymembers&cmtitle=Category:" . category
          . "&cmlimit=500&format=xml" . doContinue
    UrlDownloadToFile, %url%, %fileName% ; download the url

  getFileNoDL: ; function-internal subroutine for parsing a file
    FileRead, data, %fileName% ; read file into variable
    pos = 1 ; set while-loop counter
    rxp = title="(.*?)" ; set regular expression
    While (pos := RegExMatch(data, rxp, title, pos)) ; get the contents of the title fields one-by-one
    {
      If InStr(title1, "Category:") ; if the contents contain Category:
        StringTrimLeft, title1, title1, 9 ; remove that from the contents
      StringReplace, title1, title1, |, `|, All ; escape all containing delimiters
      title1 := Dec_XML(UTF82Ansi(title1)) ; convert to ansi first and then decode html entities
      allTitles .= title1 . "|" ; add this title to list
      pos++ ; increment counter
    }
    rxp = cmcontinue="(.*?)" ; set regular expression
    If RegExMatch(data, rxp, next) ; when found
    {
      doContinue = &cmcontinue=%next1% ; set continuation string to add to url
      Goto getFile ; go to function-internal subroutine to redownload and parse
    }
    StringTrimRight, allTitles, allTitles, 1 ; remove last delimiter from result
  Return allTitles ; return result
}

; function to convert html entities to ansi equivalents
Dec_XML(str)
{
   Loop
      If RegexMatch(str, "S)(&#(\d+);)", dec)
         StringReplace, str, str, %dec1%, % Chr(dec2), All
      Else If   RegexMatch(str, "Si)(&#x([\da-f]+);)", hex)
         StringReplace, str, str, %hex1%, % Chr("0x" . hex2), All
      Else
         Break
   StringReplace, str, str, &nbsp;, %A_Space%, All
   StringReplace, str, str, &quot;, ", All
   StringReplace, str, str, &apos;, ', All
   StringReplace, str, str, &lt;,   <, All
   StringReplace, str, str, &gt;,   >, All
   StringReplace, str, str, &amp;,  &, All
   return, str
}

; function to uri-encode input string
Enc_Uri(str)
{
   f = %A_FormatInteger%
   SetFormat, Integer, Hex
   If RegExMatch(str, "^\w+:/{0,2}", pr)
      StringTrimLeft, str, str, StrLen(pr)
   StringReplace, str, str, `%, `%25, All
   Loop
      If RegExMatch(str, "i)[^\w\.~%/:]", char)
         StringReplace, str, str, %char%, % "%" . SubStr(Asc(char),3), All
      Else Break
   SetFormat, Integer, %f%
   Return, pr . str
}

; function to convert unicode to ansi text
UTF82Ansi(zString)
{
  Ansi2Unicode(zString, wString, 65001)
  Unicode2Ansi(wString, sString, 0)
  Return sString
}

; helper function for unicode to ansi function
Ansi2Unicode(ByRef sString, ByRef wString, CP = 0)
{
  nSize := DllCall("MultiByteToWideChar", "Uint", CP
    , "Uint", 0, "Uint", &sString, "int", -1
    , "Uint", 0, "int", 0)
  VarSetCapacity(wString, nSize * 2)
  DllCall("MultiByteToWideChar"
    , "Uint", CP, "Uint", 0, "Uint", &sString, "int", -1
    , "Uint", &wString, "int", nSize)
}

; helper function for unicode to ansi function
Unicode2Ansi(ByRef wString, ByRef sString, CP = 0)
{
  nSize := DllCall("WideCharToMultiByte"
    , "Uint", CP, "Uint", 0, "Uint", &wString, "int", -1
    , "Uint", 0, "int", 0, "Uint", 0, "Uint", 0)
  VarSetCapacity(sString, nSize)
  DllCall("WideCharToMultiByte"
    , "Uint", CP, "Uint", 0, "Uint", &wString, "int", -1
    , "str", sString, "int", nSize, "Uint", 0, "Uint", 0)
}

; subroutine called when user closes the gui
GuiClose:
  ExitApp ; exit the script
Return
