#MyApp="MyLittleApp"
Mutex=CreateMutex_(0,1,#MyApp)
If GetLastError_()=#ERROR_ALREADY_EXISTS
  MessageRequester(#MyApp,"One instance is already started.")
  End
EndIf

; Main code executes here

ReleaseMutex_(Mutex)
End
