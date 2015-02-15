Function readFile(theFile As FolderItem, txtEncode As TextEncoding = Nil) As String
  Dim fileContents As String
  Dim tis As TextInputStream
  tis = tis.Open(theFile)
  fileContents = tis.ReadAll(txtEncode)
  tis.Close
  Return fileContents

Exception err As NilObjectException
  MsgBox("File Not Found.")
End Function
