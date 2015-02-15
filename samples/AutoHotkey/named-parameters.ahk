MyFunc( "Val=0, w=1024, Text=The Quick Brown Fox, newVar=I'm New" )

MyFunc( _overrides="" ) {
 Static x=5, y=5, w=100, h=100, Count
 Name:="AutoHotkey", Type:="Scripting", Text:="qwerty", Val:=True

 Loop, Parse, _overrides,`,=, %A_Space%  ; Override routine for Local/Static variables
   A_Index & 1  ? (_:=A_LoopField) : (%_%:=A_LoopField)

Listvars
 WinWaitClose, %A_ScriptFullPath%
}
