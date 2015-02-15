#MaxWorktime=8000 ; "Workday" in msec

; Structure that each thread uses
Structure MyIO
  ThreadID.i
  Semaphore_Joining.i
  Semaphore_Release.i
  Semaphore_Deliver.i
  Semaphore_Leaving.i
EndStructure

; Array of used threads
Global Dim Comm.MyIO(0)

; Master loop synchronizing the threads via semaphores
Procedure CheckPoint()
  Protected i, j, maxthreads=ArraySize(Comm())
  Protected Worker_count, Deliver_count
  Repeat
    For i=1 To maxthreads
      With Comm(i)
        If TrySemaphore(\Semaphore_Leaving)
          Worker_count-1
        ElseIf TrySemaphore(\Semaphore_Deliver)
          Deliver_count+1
          If Deliver_count=Worker_count
            PrintN("All Workers reported in, starting next task.")
            Deliver_count=0
            For j=1 To maxthreads
              SignalSemaphore(Comm(j)\Semaphore_Release)
            Next j
          EndIf
        ElseIf TrySemaphore(\Semaphore_Joining)
          PrintN("A new Worker joined the force.")
          Worker_count+1: SignalSemaphore(\Semaphore_Release)
        ElseIf Worker_count=0
          ProcedureReturn
        EndIf
      Next i
    EndWith
  ForEver
  StartAll=0
EndProcedure

; A worker thread, all orchestrated by the Checkpoint() routine
Procedure Worker(ID)
  Protected EndTime=ElapsedMilliseconds()+#MaxWorktime, n
  With Comm(ID)
    SignalSemaphore(\Semaphore_Joining)
    Repeat
      Repeat ; Use a non-blocking semaphore check to avoid dead-locking at shutdown.
        If ElapsedMilliseconds()>EndTime
          SignalSemaphore(\Semaphore_Leaving)
          PrintN("Thread #"+Str(ID)+" is done.")
          ProcedureReturn
        EndIf
        Delay(1)
      Until TrySemaphore(\Semaphore_Release)
      n=Random(1000)
      PrintN("Thread #"+Str(ID)+" will work for "+Str(n)+" msec.")
      Delay(n): PrintN("Thread #"+Str(ID)+" delivering")
      SignalSemaphore(\Semaphore_Deliver)
    ForEver
  EndWith
EndProcedure

; User IO & init
If OpenConsole()
  Define i, j
  Repeat
    Print("Enter number of workers to use [2-2000]: ")
    j=Val(Input())
  Until j>=2 And j<=2000
  ReDim Comm(j)
  For i=1 To j
    With Comm(i)
      \Semaphore_Release =CreateSemaphore()
      \Semaphore_Joining =CreateSemaphore()
      \Semaphore_Deliver =CreateSemaphore()
      \Semaphore_Leaving =CreateSemaphore()
      \ThreadID = CreateThread(@Worker(),i)
    EndWith
  Next
  PrintN("Work started, "+Str(j)+" workers has been called.")
  CheckPoint()
  Print("Press ENTER to exit"): Input()
EndIf
