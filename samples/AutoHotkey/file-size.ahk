FileGetSize, FileSize, input.txt  ; Retrieve the size in bytes.
MsgBox, Size of input.txt is %FileSize% bytes
FileGetSize, FileSize, \input.txt, K  ; Retrieve the size in Kbytes.
MsgBox, Size of \input.txt is %FileSize% Kbytes
