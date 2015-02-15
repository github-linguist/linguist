Global mutex = CreateMutex()

Procedure Printer(*str)
	LockMutex(mutex)
	PrintN( PeekS(*str) )
	UnlockMutex(mutex)
EndProcedure

If OpenConsole()
	LockMutex(mutex)
	thread1 = CreateThread(@Printer(), @"Enjoy")
	thread2 = CreateThread(@Printer(), @"Rosetta")
	thread3 = CreateThread(@Printer(), @"Code")
	UnlockMutex(mutex)
	
	WaitThread(thread1)
	WaitThread(thread2)
	WaitThread(thread3)
	
	Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
	Input()
	
	CloseConsole()
EndIf

FreeMutex(mutex)
