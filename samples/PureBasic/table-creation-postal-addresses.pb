UseSQLiteDatabase()
Procedure CheckDatabaseUpdate(Database, Query$)
   Result = DatabaseUpdate(Database, Query$)
   If Result = 0
      Print(DatabaseError())
   EndIf
   ProcedureReturn Result
EndProcedure
openconsole()
DatabaseFile$ = GetCurrentDirectory()+"/rosettadb.sdb"
If CreateFile(0, DatabaseFile$)
   CloseFile(0)
    If OpenDatabase(0, DatabaseFile$, "", "")
      CheckDatabaseUpdate(0,"CREATE TABLE address ( addrID INTEGER PRIMARY KEY AUTOINCREMENT,	addrStreet TEXT Not NULL, addrCity TEXT Not NULL, addrState TEXT Not NULL, addrZIP TEXT Not NULL)")
      CloseDatabase(0)
   Else
      print("Can't open database !")
   EndIf
Else
   print("Can't create the database file !")
EndIf
closeconsole()
