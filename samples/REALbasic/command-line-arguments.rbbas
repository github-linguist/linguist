Function Run(args() as String) As Integer
  For each arg As String In args
    Stdout.WriteLine(arg)
  Next
End Function
