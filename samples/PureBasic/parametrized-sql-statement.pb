UseSQLiteDatabase()

DatabaseFile$ = GetTemporaryDirectory()+"/Batadase.sqt"
; all kind of variables for the given case
 table$ = "players"
 name$ = "Smith, Steve"
 score.w = 42
 active$ ="TRUE"
 jerseynum.w =99

 If OpenDatabase(0, DatabaseFile$, "", "")
    Result =  DatabaseUpdate((0, "UPDATE "+table$+" SET name = '"+name$+"', score = '"+Str(score)+"', active = '"+active$+"' WHERE jerseyNum = "+Str(num)+";")
     If Result = 0
        Debug DatabaseError()
     EndIf
    CloseDatabase(0)
 Else
    Debug "Can't open database !"
 EndIf
