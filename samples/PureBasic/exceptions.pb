Procedure ErrorHandler()
  MessageRequester("Exception test", "The following error happened: " + ErrorMessage())
EndProcedure

MessageRequester("Exception test", "Test start")

OnErrorCall(@ErrorHandler())

RaiseError(#PB_OnError_InvalidMemory) ;a custom error# can also be used here depending on the OS being compiled for
