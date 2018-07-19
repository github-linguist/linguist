OnMessage(0x4a, "PrintMonitor")
SetTimer, print2, 400

print1:
  print("Old Mother Goose")
  print("When she wanted to wander,")
  print("Would ride through the air")
  print("On a very fine gander.")
  print("Jack's mother came in,")
  print("And caught the goose soon,")
  print("And mounting its back,")
  print("Flew up to the moon.")
Return

print2:
  SetTimer, print2, Off
  print("Humpty Dumpty sat on a wall.")
  print("Humpty Dumpty had a great fall.")
  print("All the king's horses and all the king's men")
  print("Couldn't put Humpty together again.")
Return

print(message)
{
  Static StringToSend
  StringToSend := message
  Gui +LastFound
  VarSetCapacity(CopyDataStruct, 12, 0)
  NumPut(StrLen(StringToSend) + 1, CopyDataStruct, 4)
  NumPut(&StringToSend, CopyDataStruct, 8)
  SendMessage, 0x4a, 0, &CopyDataStruct
  If ErrorLevel
    MsgBox out of ink
  Sleep, 200
  Return
}

PrintMonitor(wParam, lParam, msg)
{
  Static ink = 5
  Global printed
  Critical
  If ink
  {
    StringAddress := NumGet(lParam + 8)
    StringLength := DllCall("lstrlen", UInt, StringAddress)
    VarSetCapacity(CopyOfData, StringLength)
    DllCall("lstrcpy", "str", CopyOfData, "uint", StringAddress)
    printed .= "primaryprinter: " . CopyOfData . "`n"
    ToolTip, primary printer`n: %printed%
    ink--
  }
  Else
  {
    OnMessage(0x4a, "Reserve")
    print(CopyOfData)
  }
}

Reserve(wParam, lParam, msg)
{
  Static ink = 5
  Global printed
  Critical
  If ink
  {
    StringAddress := NumGet(lParam + 8)
    StringLength := DllCall("lstrlen", UInt, StringAddress)
    VarSetCapacity(CopyOfData, StringLength)
    DllCall("lstrcpy", "str", CopyOfData, "uint", StringAddress)
    printed .= "reserveprinter: " . CopyOfData . "`n"
    ToolTip, Reserve printer`n: %printed%
    ink--
  }
  Else
    Return -1
}
