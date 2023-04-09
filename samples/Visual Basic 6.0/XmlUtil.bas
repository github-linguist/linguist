Attribute VB_Name = "XmlUtil"
'[XmlUtil.bas]

'
' XML Parser by Jason Thorn (Fork by Alex Dragokas)
'

' Fork v1.4
' - added .NodeValueByName
' - removed err.raise when trying to parse empty file
' - added .LoadFile method (returns FALSE, if error happens, or file is empty.
' - .LoadData is now a function (TRUE, if success with loading)
' - improved UTF16 LE format detection.
'

' Fork v1.3 [28.11.2017]
' - added all possible error handlers
' - replaced error handlers based on Err.raise by separate function (ErrorMsg), just because I don't like at all when class raises runtime error.
'If you want to form a code logic according to critical errors, just add anything like global "LastErrorCode" variable to that function, or put Err.Raise once.
' - Fixed the range of cyrillic characters (Russian and Ukrainian) for tag names.
' - Added support of 'CDATA' type values
' - Removed attempt to serialize empty string.
'
' Fork v1.2 [27.10.2017]
'
' - added recognition of UTF-16 LE xml.
' - added protection against infinite loop, just in case.
' - fixed bug when empty tag /> could not be identified.
'
' Fork v1.1 [23.11.2015]
'
' - added "<?xml" header recognition
' - added .Value to CXmlElement
' - added .NodeCount to CXmlElement
' - added .NodeByName with support of specifying multiple nodes devided by backslash.
'

' // TODO: adding support of 'Entities', see:
' https://www.w3resource.com/xml/entities.php
' https://stackoverflow.com/questions/2784183/what-does-cdata-in-xml-mean/2784294#2784294

Option Explicit

' White Space Characters
Public Const ascSpace As Byte = 32
Public Const ascTab As Byte = 9
Public Const ascCr As Byte = 13
Public Const ascLf As Byte = 10

' Tag Characters
Public Const ascTagBegin As Byte = 60    '<
Public Const ascTagEnd As Byte = 62      '>
Public Const ascTagTerm As Byte = 47     '/
Public Const ascAmper As Byte = 38       '&
Public Const ascSemiColon As String = 59 ';

' Letter Characters (Begining And Ending for Simplicity)
Public Const ascLowerFirst As Byte = 97 'a
Public Const ascLowerLast As Byte = 122 'z
Public Const ascUpperFirst As Byte = 65 'A
Public Const ascUpperLast As Byte = 90  'Z
Public Const ascUnderScore As Byte = 95 '_
Public Const ascColon As Byte = 58      ':

' Digit Characters
Public Const ascNumFirst As Byte = 48   '0
Public Const ascNumLast As Byte = 57    '9

' Other Characters
Public Const ascEquals As Byte = 61     ' =
Public Const ascApos As Byte = 39       ' Single Quote
Public Const ascQuote As Byte = 34      ' Double Quote
Public Const ascPound As Byte = 35      ' #
Public Const ascSquareBracketOpen As Byte = 91  ' [
Public Const ascSquareBracketClose As Byte = 93 ' ]

' Special Strings
Public Const strAmp As String = "amp"           '&
Public Const strLessThan As String = "lt"       '<
Public Const strMoreThan As String = "gt"       '>
Public Const strApostrophe As String = "apos"   ''
Public Const strQuote As String = "quot"        '"
Public Const strTagCDataBegin As String = "<![CDATA["
Public Const strTagCDataEnd As String = "]]>"

Public Function DecodeEscape(Data() As Integer, Start As Long) As String
    On Error GoTo Err_Trap
    
    Do      ' Until we find a semicolon
        Start = Start + 1
        If Data(Start) = ascSemiColon Then _
            Exit Do
        DecodeEscape = DecodeEscape & ChrW$(Data(Start))
    Loop While Start <= UBound(Data)
    
    Select Case DecodeEscape
        Case strAmp
            DecodeEscape = "&"
            
        Case strApostrophe
            DecodeEscape = "'"
            
        Case strLessThan
            DecodeEscape = "<"
            
        Case strMoreThan
            DecodeEscape = ">"
            
        Case strQuote
            DecodeEscape = """"
            
        Case Else
            If Data(Start - Len(DecodeEscape)) = ascPound Then
                ' Numeric Escape Sequence
                If Data(Start - (Len(DecodeEscape) + 1)) = AscW("x") Then
                    ' Hexadecimal
                    DecodeEscape = Right$(DecodeEscape, Len(DecodeEscape) - 2)
                Else
                    ' Decimal
                    DecodeEscape = Right$(DecodeEscape, Len(DecodeEscape) - 1)
                End If
            Else
                ' Custom Entity Reference
                ' Not Currently Supported
                DecodeEscape = vbNullString
            End If
    End Select
Exit Function

Err_Trap:
    Select Case Error
        ' Exceptions Raised:
        Case 9
            'Unexpected End of Data [array index out of bounds]
            ErrorMsg Err, "XmlUtil.DecodeEscape", "Unexpected end of data"
        
        Case Else
        ' Log all other errors
            ErrorMsg Err, "XmlUtil.DecodeEscape"
        
    End Select
    If inIDE Then Stop: Resume Next
End Function

' Parses a value contained within quotes
' Start identifies the begining quote and
' will identify the closing quote on exit
Public Function ParseValue(Data() As Integer, Start As Long) As String
    Dim bEnd As Boolean
    Dim QuoteChar As Byte
    
    On Error GoTo Err_Trap
    
    QuoteChar = Data(Start)
    
    Do
        Select Case Data(Start)
            Case QuoteChar
                bEnd = Not bEnd
                If Not bEnd Then Exit Do
            
            Case Is <> ascTagBegin, Is <> ascAmper
                ParseValue = ParseValue & ChrW$(Data(Start))
                
            Case ascAmper
                ParseValue = ParseValue & DecodeEscape(Data(), Start)
            
            Case Else
                ' The only other case is the Begin Tag which is invalid in this context
                
        End Select
        Start = Start + 1
    Loop While Start <= UBound(Data)
Exit Function

Err_Trap:
    Select Case Error
        ' Exceptions Raised:
        Case 9
            'Unexpected End of Data [array index out of bounds]
            ErrorMsg Err, "XmlUtil.ParseValue", "Unexpected end of data"

        Case Else
            ' Log all other errors
            ErrorMsg Err, "XmlUtil.ParseValue"
    End Select
    If inIDE Then Stop: Resume Next
End Function

' Start Identifies the First Character to Check
' Upon completion, Start should point to the first
' non-delimitng character after the Name Value is read
Public Function ParseName(Data() As Integer, Start As Long) As String
    Dim bEnd As Boolean
    
    On Error GoTo Err_Trap
    
    Do
        Select Case Data(Start)
            ' Delimitng Characters
            Case ascSpace, ascTab, ascCr, ascLf, ascEquals, ascSemiColon
                bEnd = True
                
            Case ascTagEnd, ascApos, ascQuote
                If Data(Start - 1) = ascTagTerm Then Start = Start - 1 'to support />
                Exit Do
                
            ' Letter Characters
            Case ascUpperFirst To ascUpperLast, _
                 ascLowerFirst To ascLowerLast, _
                 ascUnderScore, ascColon, _
                 ascNumFirst To ascNumLast, _
                 &H41& To &H5A&, _
                 &H61& To &H7A&, _
                 &HC0& To &HFF&, _
                 &HB7&, _
                 &HA5&, &HA8&, &HAA&, &HAF&, &HB2&, &HB3&, &HB4&, &HB8&, &HBA&, &HBF&
                
                'A-Z
                'a-z
                'À-ÿ
                '&HB7
                '¥,¨,ª,¯,²,³,´,¸,º,¿
                
                If bEnd Then
                    Exit Do
                Else
                    ParseName = ParseName & ChrW$(Data(Start))
                End If
                
            Case Else
                ' Error . . . Normally not too many charater
                ' types can be used for the Name Identifier
                
        End Select
        Start = Start + 1
    Loop While Start <= UBound(Data)
Exit Function

Err_Trap:
    Select Case Error
        ' Exceptions Raised:
        Case 9
            'Unexpected End of Data [array index out of bounds]
            ErrorMsg Err, "XmlUtil.ParseName", "Unexpected end of data"
        
        Case Else
            ' Log all other errors
            ErrorMsg Err, "XmlUtil.ParseName"
    End Select
    If inIDE Then Stop: Resume Next
End Function

''// Common error handler
'Public Function ErrorMsg(Error As ErrObject, sFunctionName As String, ParamArray aText())
'    Dim i As Long
'    Dim s As String
'    For i = 0 To UBound(aText)
'        s = s & " " & aText(i)
'    Next
'    Debug.Print "Error: " & Error.Number & " in '" & sFunctionName & "' - " & Error.Description & IIf(Len(s) > 0, " - " & s, vbnullstring)
'End Function
'
'Public Function InIDE() As Boolean
'    InIDE = (App.LogMode = 0)
'End Function
