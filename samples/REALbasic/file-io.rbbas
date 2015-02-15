Sub WriteToFile(input As FolderItem, output As FolderItem)
  Dim tis As TextInputStream
  Dim tos As TextOutputStream
  tis = tis.Open(input)
  tos = tos.Create(output)
  While Not tis.EOF
    tos.WriteLine(tis.ReadLine)
  Wend
  tis.Close
  tos.Close
End Sub
