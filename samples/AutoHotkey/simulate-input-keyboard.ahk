run, cmd /k
WinWait, ahk_class ConsoleWindowClass
controlsend, ,hello console, ahk_class ConsoleWindowClass
