Attribute VB_Name = "ErrorHandling"
Option Explicit

Public Sub RaiseError(errNumber As Integer, Optional errSource As String = "", Optional errDescription As String = "")
    If errSource = "" Then
        'set default values
        errSource = Err.Source
        errDescription = Err.Description
    End If
    Err.Raise vbObjectError + errNumber, errSource, errDescription
End Sub


Public Sub handleError(Optional errLocation As String = "")
    Dim errorMessage As String
    errorMessage = "Error in " & errLocation & ", [" & Err.Source & "] : error number " & Err.Number & vbNewLine & Err.Description
    Debug.Print errorMessage
    MsgBox errorMessage, vbCritical, "vbaDeveloper ErrorHandler"
End Sub

