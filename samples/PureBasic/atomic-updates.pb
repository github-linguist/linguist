#Buckets=9
#TotalAmount=200
Global Dim Buckets(#Buckets)
Global BMutex=CreateMutex()
Global Quit=#False

Procedure max(x,y)
  If x>=y:  ProcedureReturn x
  Else:     ProcedureReturn y
  EndIf
EndProcedure

Procedure Move(WantedAmount, From, Dest)
  Protected RealAmount
  If from<>Dest
    LockMutex(BMutex)
    RealAmount=max(0, Buckets(from)-WantedAmount)
    Buckets(From)-RealAmount
    Buckets(Dest)+RealAmount
    UnlockMutex(BMutex)
  EndIf
  ProcedureReturn RealAmount
EndProcedure

Procedure Level(A,B)
  Protected i, j, t
  If A<>B
    LockMutex(BMutex)
    t=Buckets(A)+Buckets(B)
    i=t/2: j=t-i
    Buckets(A)=i
    Buckets(B)=j
    UnlockMutex(BMutex)
  EndIf
EndProcedure

Procedure DoInvent(Array A(1))
  Protected i, sum
  LockMutex(BMutex)
  For i=0 To ArraySize(Buckets())
    A(i)=Buckets(i)
    sum+A(i)
  Next i
  UnlockMutex(BMutex)
  ProcedureReturn sum
EndProcedure

Procedure MixingThread(arg)
  Repeat
    Move(Random(#TotalAmount),Random(#Buckets),Random(#Buckets))
  Until Quit
EndProcedure

Procedure LevelingThread(arg)
  Repeat
    Level(Random(#Buckets),Random(#Buckets))
  Until Quit
EndProcedure

If OpenWindow(0,0,0,100,150,"Atomic updates",#PB_Window_SystemMenu)
  Define Thread1=CreateThread(@MixingThread(),0)
  Define Thread2=CreateThread(@MixingThread(),0)
  Define i, Event
  Dim Inventory(#Buckets)
  ; Set up a small GUI
  For i=0 To 9
    TextGadget(i, 0,i*15,50, 15,"Bucket #"+Str(i))
  Next i
  TextGadget(10,55,135,40,15,"=")
  AddWindowTimer(0,0,500)
  Buckets(0)=#TotalAmount
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Timer
      i=DoInvent(Inventory())
      SetGadgetText(10,"="+Str(i))
      For i=0 To #Buckets
        SetGadgetText(i, Str(Inventory(i)))
      Next i
    EndIf
  Until Event=#PB_Event_CloseWindow
  Quit=#True  ; Tell threads to shut down
  WaitThread(Thread1): WaitThread(Thread2)
EndIf
