global msgXtra

on clickWindowButton windowRef, buttonNum
  set WM_LBUTTONDOWN = 513
  set WM_LBUTTONUP = 514
  
  set buttonRefs = baChildWindowList(windowRef, "Button", "", False)
  if buttonNum <= the count of buttonRefs then
    set buttonRef = buttonRefs.getAt(buttonNum)
    msgXtra.send_msg(buttonRef, WM_LBUTTONDOWN, 0, 0)
    msgXtra.send_msg(buttonRef, WM_LBUTTONUP, 0, 0)
    return True
  end if
  return False
end

on findDialog windowTitle, exactMatch
  set dialogClass = "#32770"
  set dialogRefs = baWindowList(dialogClass, windowTitle, exactMatch)
  if the count of dialogRefs > 0 then return dialogRefs.getAt(1)
  return 0
end

on dismissDialog windowTitle, exactMatch, buttonIndex
  set dialogRef = findDialog(windowTitle, exactMatch)
  if dialogRef then return clickWindowButton(dialogRef, buttonIndex)
end

on dismissPopupDialogs
  dismissDialog("Director Player Error", True, 1)
  dismissDialog("Where is" && QUOTE, False, 2)
end

on prepareMovie
  if the commandLine = EMPTY then
    alert "This is a tool used by Cast Ripper. It should not be run directly."
    halt()
  else
    set the title of the window = the commandLine
  end if
  set msgXtra = new xtra("msg")
end