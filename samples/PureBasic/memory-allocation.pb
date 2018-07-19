*buffer=AllocateMemory(20)

*newBuffer = ReAllocateMemory(*buffer, 2000) ;increase size of buffer
;*buffer value is still valid if newBuffer wasn't able to be reallocated
If *newBuffer <> 0
  *buffer = *newBuffer : *newBuffer = 0
EndIf

FreeMemory(*buffer)


size=20
; allocate an image for use with image functions
CreateImage(1,size,size)
FreeImage(1)
