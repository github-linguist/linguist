#Threads=10
#Parallels=3
Global Semaphore=CreateSemaphore(#Parallels)

Procedure Worker(*arg.i)
  WaitSemaphore(Semaphore)
  Debug "Thread #"+Str(*arg)+" active."
  Delay(Random(2000))
  SignalSemaphore(Semaphore)
EndProcedure

; Start a multi-thread based work
Dim thread(#Threads)
For i=0 To #Threads
  thread(i)=CreateThread(@Worker(),i)
Next
Debug "Launcher done."

; Wait for all threads to finish before closing down
For i=0 To #Threads
  If IsThread(i)
    WaitThread(i)
  EndIf
Next
