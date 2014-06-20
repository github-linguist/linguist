Dim dbFile As FolderItem
Dim db As New SQLiteDatabase
dbFile = GetFolderItem("Employees.sqlite")
db.DatabaseFile = dbFile
If db.Connect Then
  db.SQLExecute("BEGIN TRANSACTION")
  db.SQLExecute ("INSERT INTO Employees (Name,Job,YearJoined) VALUES "_
    +"('Dr.Strangelove','Advisor',1962)")
  If db.Error then
    MsgBox("Error: " + db.ErrorMessage)
    db.Rollback
  Else
    db.Commit
  End If
Else
  MsgBox("The database couldn't be opened. Error: " + db.ErrorMessage)
End If
