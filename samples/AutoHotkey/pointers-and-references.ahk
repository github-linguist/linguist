VarSetCapacity(var, 100)   ; allocate memory
NumPut(87, var, 0, "Char") ; store 87 at offset 0
MsgBox % NumGet(var, 0, "Char") ; get character at offset 0 (87)
MsgBox % &var   ; address of contents pointed to by var structure
MsgBox % *&var ; integer at address of var contents  (87)
