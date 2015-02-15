; Example: The following is a working script that displays a summary of all top-level windows.

; For performance and memory conservation, call RegisterCallback() only once for a given callback:
if not EnumAddress  ; Fast-mode is okay because it will be called only from this thread:
    EnumAddress := RegisterCallback("EnumWindowsProc", "Fast")

DetectHiddenWindows On  ; Due to fast-mode, this setting will go into effect for the callback too.

; Pass control to EnumWindows(), which calls the callback repeatedly:
DllCall("EnumWindows", UInt, EnumAddress, UInt, 0)
MsgBox %Output%  ; Display the information accumulated by the callback.

EnumWindowsProc(hwnd, lParam)
{
    global Output
    WinGetTitle, title, ahk_id %hwnd%
    WinGetClass, class, ahk_id %hwnd%
    if title
        Output .= "HWND: " . hwnd . "`tTitle: " . title . "`tClass: " . class . "`n"
    return true  ; Tell EnumWindows() to continue until all windows have been enumerated.
}
