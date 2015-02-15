Enumeration
  #Write
  #Done
EndEnumeration

Structure commblock
  txtline.s
  Order.i
EndStructure

Global MessageSent=CreateSemaphore()
Global LineWritten=CreateSemaphore()
Global LinesWritten, com.commblock

Procedure Writer(arg)
  Repeat
    WaitSemaphore(MessageSent)
    If com\Order=#Write
      PrintN(com\txtline)
      LinesWritten+1
    EndIf
    SignalSemaphore(LineWritten)
  Until com\Order=#Done
EndProcedure

Procedure Reader(arg)
  Protected File=ReadFile(#PB_Any,OpenFileRequester("","input.txt","",0))
  While file And Not Eof(file)
    com\txtline=ReadString(File)
    com\Order=#Write
    SignalSemaphore(MessageSent)
    WaitSemaphore(LineWritten)
  Wend
  com\Order=#Done
  SignalSemaphore(MessageSent)
  WaitSemaphore(LineWritten)
  PrintN(Str(LinesWritten)+" lines written.")
EndProcedure

If OpenConsole()
  Define Thread1=CreateThread(@Reader(),0)
  Define Thread2=CreateThread(@Writer(),0)
  WaitThread(Thread1) And WaitThread(Thread2)
  Print("Press Enter to exit"):Input()
EndIf
