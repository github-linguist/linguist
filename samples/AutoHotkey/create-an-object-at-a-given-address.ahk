; Create a variable with 4 bytes size and show it's machine address.
VarSetCapacity(var, 4, 0)
pAddress := &var
MsgBox Machine address: %pAddress%

; pAddress contains the memory address.
; Write a number and read it back.
NumPut(123456, pAddress+0, 0, "UInt")
MsgBox % "Contents of *pAddress: " . NumGet(pAddress+0, 0, "UInt")
