Attribute VB_Name = "JsonConverter"
''
' VBA-JSON v2.3.1
' (c) Tim Hall - https://github.com/VBA-tools/VBA-JSON
'
' JSON Converter for VBA
'
' Errors:
' 10001 - JSON parse error
'
' @class JsonConverter
' @author tim.hall.engr@gmail.com
' @license MIT (http://www.opensource.org/licenses/mit-license.php)
'' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
'
' Based originally on vba-json (with extensive changes)
' BSD license included below
'
' JSONLib, http://code.google.com/p/vba-json/
'
' Copyright (c) 2013, Ryo Yokoyama
' All rights reserved.
'
' Redistribution and use in source and binary forms, with or without
' modification, are permitted provided that the following conditions are met:
'     * Redistributions of source code must retain the above copyright
'       notice, this list of conditions and the following disclaimer.
'     * Redistributions in binary form must reproduce the above copyright
'       notice, this list of conditions and the following disclaimer in the
'       documentation and/or other materials provided with the distribution.
'     * Neither the name of the <organization> nor the
'       names of its contributors may be used to endorse or promote products
'       derived from this software without specific prior written permission.
'
' THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
' ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
' DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
' DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
' (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
' LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
' ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
' (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
' SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
Option Explicit

' === VBA-UTC Headers
#If Mac Then

#If VBA7 Then

' 64-bit Mac (2016)
Private Declare PtrSafe Function utc_popen Lib "/usr/lib/libc.dylib" Alias "popen" _
    (ByVal utc_Command As String, ByVal utc_Mode As String) As LongPtr
Private Declare PtrSafe Function utc_pclose Lib "/usr/lib/libc.dylib" Alias "pclose" _
    (ByVal utc_File As LongPtr) As LongPtr
Private Declare PtrSafe Function utc_fread Lib "/usr/lib/libc.dylib" Alias "fread" _
    (ByVal utc_Buffer As String, ByVal utc_Size As LongPtr, ByVal utc_Number As LongPtr, ByVal utc_File As LongPtr) As LongPtr
Private Declare PtrSafe Function utc_feof Lib "/usr/lib/libc.dylib" Alias "feof" _
    (ByVal utc_File As LongPtr) As LongPtr

#Else

' 32-bit Mac
Private Declare Function utc_popen Lib "libc.dylib" Alias "popen" _
    (ByVal utc_Command As String, ByVal utc_Mode As String) As Long
Private Declare Function utc_pclose Lib "libc.dylib" Alias "pclose" _
    (ByVal utc_File As Long) As Long
Private Declare Function utc_fread Lib "libc.dylib" Alias "fread" _
    (ByVal utc_Buffer As String, ByVal utc_Size As Long, ByVal utc_Number As Long, ByVal utc_File As Long) As Long
Private Declare Function utc_feof Lib "libc.dylib" Alias "feof" _
    (ByVal utc_File As Long) As Long

#End If

#ElseIf VBA7 Then

' http://msdn.microsoft.com/en-us/library/windows/desktop/ms724421.aspx
' http://msdn.microsoft.com/en-us/library/windows/desktop/ms724949.aspx
' http://msdn.microsoft.com/en-us/library/windows/desktop/ms725485.aspx
Private Declare PtrSafe Function utc_GetTimeZoneInformation Lib "kernel32" Alias "GetTimeZoneInformation" _
    (utc_lpTimeZoneInformation As utc_TIME_ZONE_INFORMATION) As Long
Private Declare PtrSafe Function utc_SystemTimeToTzSpecificLocalTime Lib "kernel32" Alias "SystemTimeToTzSpecificLocalTime" _
    (utc_lpTimeZoneInformation As utc_TIME_ZONE_INFORMATION, utc_lpUniversalTime As utc_SYSTEMTIME, utc_lpLocalTime As utc_SYSTEMTIME) As Long
Private Declare PtrSafe Function utc_TzSpecificLocalTimeToSystemTime Lib "kernel32" Alias "TzSpecificLocalTimeToSystemTime" _
    (utc_lpTimeZoneInformation As utc_TIME_ZONE_INFORMATION, utc_lpLocalTime As utc_SYSTEMTIME, utc_lpUniversalTime As utc_SYSTEMTIME) As Long

#Else

Private Declare Function utc_GetTimeZoneInformation Lib "kernel32" Alias "GetTimeZoneInformation" _
    (utc_lpTimeZoneInformation As utc_TIME_ZONE_INFORMATION) As Long
Private Declare Function utc_SystemTimeToTzSpecificLocalTime Lib "kernel32" Alias "SystemTimeToTzSpecificLocalTime" _
    (utc_lpTimeZoneInformation As utc_TIME_ZONE_INFORMATION, utc_lpUniversalTime As utc_SYSTEMTIME, utc_lpLocalTime As utc_SYSTEMTIME) As Long
Private Declare Function utc_TzSpecificLocalTimeToSystemTime Lib "kernel32" Alias "TzSpecificLocalTimeToSystemTime" _
    (utc_lpTimeZoneInformation As utc_TIME_ZONE_INFORMATION, utc_lpLocalTime As utc_SYSTEMTIME, utc_lpUniversalTime As utc_SYSTEMTIME) As Long

#End If

#If Mac Then

#If VBA7 Then
Private Type utc_ShellResult
    utc_Output As String
    utc_ExitCode As LongPtr
End Type

#Else

Private Type utc_ShellResult
    utc_Output As String
    utc_ExitCode As Long
End Type

#End If

#Else

Private Type utc_SYSTEMTIME
    utc_wYear As Integer
    utc_wMonth As Integer
    utc_wDayOfWeek As Integer
    utc_wDay As Integer
    utc_wHour As Integer
    utc_wMinute As Integer
    utc_wSecond As Integer
    utc_wMilliseconds As Integer
End Type

Private Type utc_TIME_ZONE_INFORMATION
    utc_Bias As Long
    utc_StandardName(0 To 31) As Integer
    utc_StandardDate As utc_SYSTEMTIME
    utc_StandardBias As Long
    utc_DaylightName(0 To 31) As Integer
    utc_DaylightDate As utc_SYSTEMTIME
    utc_DaylightBias As Long
End Type

#End If
' === End VBA-UTC

Private Type json_Options
    ' VBA only stores 15 significant digits, so any numbers larger than that are truncated
    ' This can lead to issues when BIGINT's are used (e.g. for Ids or Credit Cards), as they will be invalid above 15 digits
    ' See: http://support.microsoft.com/kb/269370
    '
    ' By default, VBA-JSON will use String for numbers longer than 15 characters that contain only digits
    ' to override set `JsonConverter.JsonOptions.UseDoubleForLargeNumbers = True`
    UseDoubleForLargeNumbers As Boolean

    ' The JSON standard requires object keys to be quoted (" or '), use this option to allow unquoted keys
    AllowUnquotedKeys As Boolean

    ' The solidus (/) is not required to be escaped, use this option to escape them as \/ in ConvertToJson
    EscapeSolidus As Boolean
End Type
Public JsonOptions As json_Options

' ============================================= '
' Public Methods
' ============================================= '

''
' Convert JSON string to object (Dictionary/Collection)
'
' @method ParseJson
' @param {String} json_String
' @return {Object} (Dictionary or Collection)
' @throws 10001 - JSON parse error
''
Public Function ParseJson(ByVal JsonString As String) As Object
    Dim json_Index As Long
    json_Index = 1

    ' Remove vbCr, vbLf, and vbTab from json_String
    JsonString = VBA.Replace(VBA.Replace(VBA.Replace(JsonString, VBA.vbCr, ""), VBA.vbLf, ""), VBA.vbTab, "")

    json_SkipSpaces JsonString, json_Index
    Select Case VBA.Mid$(JsonString, json_Index, 1)
    Case "{"
        Set ParseJson = json_ParseObject(JsonString, json_Index)
    Case "["
        Set ParseJson = json_ParseArray(JsonString, json_Index)
    Case Else
        ' Error: Invalid JSON string
        Err.Raise 10001, "JSONConverter", json_ParseErrorMessage(JsonString, json_Index, "Expecting '{' or '['")
    End Select
End Function

''
' Convert object (Dictionary/Collection/Array) to JSON
'
' @method ConvertToJson
' @param {Variant} JsonValue (Dictionary, Collection, or Array)
' @param {Integer|String} Whitespace "Pretty" print json with given number of spaces per indentation (Integer) or given string
' @return {String}
''
Public Function ConvertToJson(ByVal JsonValue As Variant, Optional ByVal Whitespace As Variant, Optional ByVal json_CurrentIndentation As Long = 0) As String
    Dim json_Buffer As String
    Dim json_BufferPosition As Long
    Dim json_BufferLength As Long
    Dim json_Index As Long
    Dim json_LBound As Long
    Dim json_UBound As Long
    Dim json_IsFirstItem As Boolean
    Dim json_Index2D As Long
    Dim json_LBound2D As Long
    Dim json_UBound2D As Long
    Dim json_IsFirstItem2D As Boolean
    Dim json_Key As Variant
    Dim json_Value As Variant
    Dim json_DateStr As String
    Dim json_Converted As String
    Dim json_SkipItem As Boolean
    Dim json_PrettyPrint As Boolean
    Dim json_Indentation As String
    Dim json_InnerIndentation As String

    json_LBound = -1
    json_UBound = -1
    json_IsFirstItem = True
    json_LBound2D = -1
    json_UBound2D = -1
    json_IsFirstItem2D = True
    json_PrettyPrint = Not IsMissing(Whitespace)

    Select Case VBA.VarType(JsonValue)
    Case VBA.vbNull
        ConvertToJson = "null"
    Case VBA.vbDate
        ' Date
        json_DateStr = ConvertToIso(VBA.CDate(JsonValue))

        ConvertToJson = """" & json_DateStr & """"
    Case VBA.vbString
        ' String (or large number encoded as string)
        If Not JsonOptions.UseDoubleForLargeNumbers And json_StringIsLargeNumber(JsonValue) Then
            ConvertToJson = JsonValue
        Else
            ConvertToJson = """" & json_Encode(JsonValue) & """"
        End If
    Case VBA.vbBoolean
        If JsonValue Then
            ConvertToJson = "true"
        Else
            ConvertToJson = "false"
        End If
    Case VBA.vbArray To VBA.vbArray + VBA.vbByte
        If json_PrettyPrint Then
            If VBA.VarType(Whitespace) = VBA.vbString Then
                json_Indentation = VBA.String$(json_CurrentIndentation + 1, Whitespace)
                json_InnerIndentation = VBA.String$(json_CurrentIndentation + 2, Whitespace)
            Else
                json_Indentation = VBA.Space$((json_CurrentIndentation + 1) * Whitespace)
                json_InnerIndentation = VBA.Space$((json_CurrentIndentation + 2) * Whitespace)
            End If
        End If

        ' Array
        json_BufferAppend json_Buffer, "[", json_BufferPosition, json_BufferLength

        On Error Resume Next

        json_LBound = LBound(JsonValue, 1)
        json_UBound = UBound(JsonValue, 1)
        json_LBound2D = LBound(JsonValue, 2)
        json_UBound2D = UBound(JsonValue, 2)

        If json_LBound >= 0 And json_UBound >= 0 Then
            For json_Index = json_LBound To json_UBound
                If json_IsFirstItem Then
                    json_IsFirstItem = False
                Else
                    ' Append comma to previous line
                    json_BufferAppend json_Buffer, ",", json_BufferPosition, json_BufferLength
                End If

                If json_LBound2D >= 0 And json_UBound2D >= 0 Then
                    ' 2D Array
                    If json_PrettyPrint Then
                        json_BufferAppend json_Buffer, vbNewLine, json_BufferPosition, json_BufferLength
                    End If
                    json_BufferAppend json_Buffer, json_Indentation & "[", json_BufferPosition, json_BufferLength

                    For json_Index2D = json_LBound2D To json_UBound2D
                        If json_IsFirstItem2D Then
                            json_IsFirstItem2D = False
                        Else
                            json_BufferAppend json_Buffer, ",", json_BufferPosition, json_BufferLength
                        End If

                        json_Converted = ConvertToJson(JsonValue(json_Index, json_Index2D), Whitespace, json_CurrentIndentation + 2)

                        ' For Arrays/Collections, undefined (Empty/Nothing) is treated as null
                        If json_Converted = "" Then
                            ' (nest to only check if converted = "")
                            If json_IsUndefined(JsonValue(json_Index, json_Index2D)) Then
                                json_Converted = "null"
                            End If
                        End If

                        If json_PrettyPrint Then
                            json_Converted = vbNewLine & json_InnerIndentation & json_Converted
                        End If

                        json_BufferAppend json_Buffer, json_Converted, json_BufferPosition, json_BufferLength
                    Next json_Index2D

                    If json_PrettyPrint Then
                        json_BufferAppend json_Buffer, vbNewLine, json_BufferPosition, json_BufferLength
                    End If

                    json_BufferAppend json_Buffer, json_Indentation & "]", json_BufferPosition, json_BufferLength
                    json_IsFirstItem2D = True
                Else
                    ' 1D Array
                    json_Converted = ConvertToJson(JsonValue(json_Index), Whitespace, json_CurrentIndentation + 1)

                    ' For Arrays/Collections, undefined (Empty/Nothing) is treated as null
                    If json_Converted = "" Then
                        ' (nest to only check if converted = "")
                        If json_IsUndefined(JsonValue(json_Index)) Then
                            json_Converted = "null"
                        End If
                    End If

                    If json_PrettyPrint Then
                        json_Converted = vbNewLine & json_Indentation & json_Converted
                    End If

                    json_BufferAppend json_Buffer, json_Converted, json_BufferPosition, json_BufferLength
                End If
            Next json_Index
        End If

        On Error GoTo 0

        If json_PrettyPrint Then
            json_BufferAppend json_Buffer, vbNewLine, json_BufferPosition, json_BufferLength

            If VBA.VarType(Whitespace) = VBA.vbString Then
                json_Indentation = VBA.String$(json_CurrentIndentation, Whitespace)
            Else
                json_Indentation = VBA.Space$(json_CurrentIndentation * Whitespace)
            End If
        End If

        json_BufferAppend json_Buffer, json_Indentation & "]", json_BufferPosition, json_BufferLength

        ConvertToJson = json_BufferToString(json_Buffer, json_BufferPosition)

    ' Dictionary or Collection
    Case VBA.vbObject
        If json_PrettyPrint Then
            If VBA.VarType(Whitespace) = VBA.vbString Then
                json_Indentation = VBA.String$(json_CurrentIndentation + 1, Whitespace)
            Else
                json_Indentation = VBA.Space$((json_CurrentIndentation + 1) * Whitespace)
            End If
        End If

        ' Dictionary
        If VBA.TypeName(JsonValue) = "Dictionary" Then
            json_BufferAppend json_Buffer, "{", json_BufferPosition, json_BufferLength
            For Each json_Key In JsonValue.Keys
                ' For Objects, undefined (Empty/Nothing) is not added to object
                json_Converted = ConvertToJson(JsonValue(json_Key), Whitespace, json_CurrentIndentation + 1)
                If json_Converted = "" Then
                    json_SkipItem = json_IsUndefined(JsonValue(json_Key))
                Else
                    json_SkipItem = False
                End If

                If Not json_SkipItem Then
                    If json_IsFirstItem Then
                        json_IsFirstItem = False
                    Else
                        json_BufferAppend json_Buffer, ",", json_BufferPosition, json_BufferLength
                    End If

                    If json_PrettyPrint Then
                        json_Converted = vbNewLine & json_Indentation & """" & json_Key & """: " & json_Converted
                    Else
                        json_Converted = """" & json_Key & """:" & json_Converted
                    End If

                    json_BufferAppend json_Buffer, json_Converted, json_BufferPosition, json_BufferLength
                End If
            Next json_Key

            If json_PrettyPrint Then
                json_BufferAppend json_Buffer, vbNewLine, json_BufferPosition, json_BufferLength

                If VBA.VarType(Whitespace) = VBA.vbString Then
                    json_Indentation = VBA.String$(json_CurrentIndentation, Whitespace)
                Else
                    json_Indentation = VBA.Space$(json_CurrentIndentation * Whitespace)
                End If
            End If

            json_BufferAppend json_Buffer, json_Indentation & "}", json_BufferPosition, json_BufferLength

        ' Collection
        ElseIf VBA.TypeName(JsonValue) = "Collection" Then
            json_BufferAppend json_Buffer, "[", json_BufferPosition, json_BufferLength
            For Each json_Value In JsonValue
                If json_IsFirstItem Then
                    json_IsFirstItem = False
                Else
                    json_BufferAppend json_Buffer, ",", json_BufferPosition, json_BufferLength
                End If

                json_Converted = ConvertToJson(json_Value, Whitespace, json_CurrentIndentation + 1)

                ' For Arrays/Collections, undefined (Empty/Nothing) is treated as null
                If json_Converted = "" Then
                    ' (nest to only check if converted = "")
                    If json_IsUndefined(json_Value) Then
                        json_Converted = "null"
                    End If
                End If

                If json_PrettyPrint Then
                    json_Converted = vbNewLine & json_Indentation & json_Converted
                End If

                json_BufferAppend json_Buffer, json_Converted, json_BufferPosition, json_BufferLength
            Next json_Value

            If json_PrettyPrint Then
                json_BufferAppend json_Buffer, vbNewLine, json_BufferPosition, json_BufferLength

                If VBA.VarType(Whitespace) = VBA.vbString Then
                    json_Indentation = VBA.String$(json_CurrentIndentation, Whitespace)
                Else
                    json_Indentation = VBA.Space$(json_CurrentIndentation * Whitespace)
                End If
            End If

            json_BufferAppend json_Buffer, json_Indentation & "]", json_BufferPosition, json_BufferLength
        End If

        ConvertToJson = json_BufferToString(json_Buffer, json_BufferPosition)
    Case VBA.vbInteger, VBA.vbLong, VBA.vbSingle, VBA.vbDouble, VBA.vbCurrency, VBA.vbDecimal
        ' Number (use decimals for numbers)
        ConvertToJson = VBA.Replace(JsonValue, ",", ".")
    Case Else
        ' vbEmpty, vbError, vbDataObject, vbByte, vbUserDefinedType
        ' Use VBA's built-in to-string
        On Error Resume Next
        ConvertToJson = JsonValue
        On Error GoTo 0
    End Select
End Function

' ============================================= '
' Private Functions
' ============================================= '

Private Function json_ParseObject(json_String As String, ByRef json_Index As Long) As Dictionary
    Dim json_Key As String
    Dim json_NextChar As String

    Set json_ParseObject = New Dictionary
    json_SkipSpaces json_String, json_Index
    If VBA.Mid$(json_String, json_Index, 1) <> "{" Then
        Err.Raise 10001, "JSONConverter", json_ParseErrorMessage(json_String, json_Index, "Expecting '{'")
    Else
        json_Index = json_Index + 1

        Do
            json_SkipSpaces json_String, json_Index
            If VBA.Mid$(json_String, json_Index, 1) = "}" Then
                json_Index = json_Index + 1
                Exit Function
            ElseIf VBA.Mid$(json_String, json_Index, 1) = "," Then
                json_Index = json_Index + 1
                json_SkipSpaces json_String, json_Index
            End If

            json_Key = json_ParseKey(json_String, json_Index)
            json_NextChar = json_Peek(json_String, json_Index)
            If json_NextChar = "[" Or json_NextChar = "{" Then
                Set json_ParseObject.Item(json_Key) = json_ParseValue(json_String, json_Index)
            Else
                json_ParseObject.Item(json_Key) = json_ParseValue(json_String, json_Index)
            End If
        Loop
    End If
End Function

Private Function json_ParseArray(json_String As String, ByRef json_Index As Long) As Collection
    Set json_ParseArray = New Collection

    json_SkipSpaces json_String, json_Index
    If VBA.Mid$(json_String, json_Index, 1) <> "[" Then
        Err.Raise 10001, "JSONConverter", json_ParseErrorMessage(json_String, json_Index, "Expecting '['")
    Else
        json_Index = json_Index + 1

        Do
            json_SkipSpaces json_String, json_Index
            If VBA.Mid$(json_String, json_Index, 1) = "]" Then
                json_Index = json_Index + 1
                Exit Function
            ElseIf VBA.Mid$(json_String, json_Index, 1) = "," Then
                json_Index = json_Index + 1
                json_SkipSpaces json_String, json_Index
            End If

            json_ParseArray.Add json_ParseValue(json_String, json_Index)
        Loop
    End If
End Function

Private Function json_ParseValue(json_String As String, ByRef json_Index As Long) As Variant
    json_SkipSpaces json_String, json_Index
    Select Case VBA.Mid$(json_String, json_Index, 1)
    Case "{"
        Set json_ParseValue = json_ParseObject(json_String, json_Index)
    Case "["
        Set json_ParseValue = json_ParseArray(json_String, json_Index)
    Case """", "'"
        json_ParseValue = json_ParseString(json_String, json_Index)
    Case Else
        If VBA.Mid$(json_String, json_Index, 4) = "true" Then
            json_ParseValue = True
            json_Index = json_Index + 4
        ElseIf VBA.Mid$(json_String, json_Index, 5) = "false" Then
            json_ParseValue = False
            json_Index = json_Index + 5
        ElseIf VBA.Mid$(json_String, json_Index, 4) = "null" Then
            json_ParseValue = Null
            json_Index = json_Index + 4
        ElseIf VBA.InStr("+-0123456789", VBA.Mid$(json_String, json_Index, 1)) Then
            json_ParseValue = json_ParseNumber(json_String, json_Index)
        Else
            Err.Raise 10001, "JSONConverter", json_ParseErrorMessage(json_String, json_Index, "Expecting 'STRING', 'NUMBER', null, true, false, '{', or '['")
        End If
    End Select
End Function

Private Function json_ParseString(json_String As String, ByRef json_Index As Long) As String
    Dim json_Quote As String
    Dim json_Char As String
    Dim json_Code As String
    Dim json_Buffer As String
    Dim json_BufferPosition As Long
    Dim json_BufferLength As Long

    json_SkipSpaces json_String, json_Index

    ' Store opening quote to look for matching closing quote
    json_Quote = VBA.Mid$(json_String, json_Index, 1)
    json_Index = json_Index + 1

    Do While json_Index > 0 And json_Index <= Len(json_String)
        json_Char = VBA.Mid$(json_String, json_Index, 1)

        Select Case json_Char
        Case "\"
            ' Escaped string, \\, or \/
            json_Index = json_Index + 1
            json_Char = VBA.Mid$(json_String, json_Index, 1)

            Select Case json_Char
            Case """", "\", "/", "'"
                json_BufferAppend json_Buffer, json_Char, json_BufferPosition, json_BufferLength
                json_Index = json_Index + 1
            Case "b"
                json_BufferAppend json_Buffer, vbBack, json_BufferPosition, json_BufferLength
                json_Index = json_Index + 1
            Case "f"
                json_BufferAppend json_Buffer, vbFormFeed, json_BufferPosition, json_BufferLength
                json_Index = json_Index + 1
            Case "n"
                json_BufferAppend json_Buffer, vbCrLf, json_BufferPosition, json_BufferLength
                json_Index = json_Index + 1
            Case "r"
                json_BufferAppend json_Buffer, vbCr, json_BufferPosition, json_BufferLength
                json_Index = json_Index + 1
            Case "t"
                json_BufferAppend json_Buffer, vbTab, json_BufferPosition, json_BufferLength
                json_Index = json_Index + 1
            Case "u"
                ' Unicode character escape (e.g. \u00a9 = Copyright)
                json_Index = json_Index + 1
                json_Code = VBA.Mid$(json_String, json_Index, 4)
                json_BufferAppend json_Buffer, VBA.ChrW(VBA.Val("&h" + json_Code)), json_BufferPosition, json_BufferLength
                json_Index = json_Index + 4
            End Select
        Case json_Quote
            json_ParseString = json_BufferToString(json_Buffer, json_BufferPosition)
            json_Index = json_Index + 1
            Exit Function
        Case Else
            json_BufferAppend json_Buffer, json_Char, json_BufferPosition, json_BufferLength
            json_Index = json_Index + 1
        End Select
    Loop
End Function

Private Function json_ParseNumber(json_String As String, ByRef json_Index As Long) As Variant
    Dim json_Char As String
    Dim json_Value As String
    Dim json_IsLargeNumber As Boolean

    json_SkipSpaces json_String, json_Index

    Do While json_Index > 0 And json_Index <= Len(json_String)
        json_Char = VBA.Mid$(json_String, json_Index, 1)

        If VBA.InStr("+-0123456789.eE", json_Char) Then
            ' Unlikely to have massive number, so use simple append rather than buffer here
            json_Value = json_Value & json_Char
            json_Index = json_Index + 1
        Else
            ' Excel only stores 15 significant digits, so any numbers larger than that are truncated
            ' This can lead to issues when BIGINT's are used (e.g. for Ids or Credit Cards), as they will be invalid above 15 digits
            ' See: http://support.microsoft.com/kb/269370
            '
            ' Fix: Parse -> String, Convert -> String longer than 15/16 characters containing only numbers and decimal points -> Number
            ' (decimal doesn't factor into significant digit count, so if present check for 15 digits + decimal = 16)
            json_IsLargeNumber = IIf(InStr(json_Value, "."), Len(json_Value) >= 17, Len(json_Value) >= 16)
            If Not JsonOptions.UseDoubleForLargeNumbers And json_IsLargeNumber Then
                json_ParseNumber = json_Value
            Else
                ' VBA.Val does not use regional settings, so guard for comma is not needed
                json_ParseNumber = VBA.Val(json_Value)
            End If
            Exit Function
        End If
    Loop
End Function

Private Function json_ParseKey(json_String As String, ByRef json_Index As Long) As String
    ' Parse key with single or double quotes
    If VBA.Mid$(json_String, json_Index, 1) = """" Or VBA.Mid$(json_String, json_Index, 1) = "'" Then
        json_ParseKey = json_ParseString(json_String, json_Index)
    ElseIf JsonOptions.AllowUnquotedKeys Then
        Dim json_Char As String
        Do While json_Index > 0 And json_Index <= Len(json_String)
            json_Char = VBA.Mid$(json_String, json_Index, 1)
            If (json_Char <> " ") And (json_Char <> ":") Then
                json_ParseKey = json_ParseKey & json_Char
                json_Index = json_Index + 1
            Else
                Exit Do
            End If
        Loop
    Else
        Err.Raise 10001, "JSONConverter", json_ParseErrorMessage(json_String, json_Index, "Expecting '""' or '''")
    End If

    ' Check for colon and skip if present or throw if not present
    json_SkipSpaces json_String, json_Index
    If VBA.Mid$(json_String, json_Index, 1) <> ":" Then
        Err.Raise 10001, "JSONConverter", json_ParseErrorMessage(json_String, json_Index, "Expecting ':'")
    Else
        json_Index = json_Index + 1
    End If
End Function

Private Function json_IsUndefined(ByVal json_Value As Variant) As Boolean
    ' Empty / Nothing -> undefined
    Select Case VBA.VarType(json_Value)
    Case VBA.vbEmpty
        json_IsUndefined = True
    Case VBA.vbObject
        Select Case VBA.TypeName(json_Value)
        Case "Empty", "Nothing"
            json_IsUndefined = True
        End Select
    End Select
End Function

Private Function json_Encode(ByVal json_Text As Variant) As String
    ' Reference: http://www.ietf.org/rfc/rfc4627.txt
    ' Escape: ", \, /, backspace, form feed, line feed, carriage return, tab
    Dim json_Index As Long
    Dim json_Char As String
    Dim json_AscCode As Long
    Dim json_Buffer As String
    Dim json_BufferPosition As Long
    Dim json_BufferLength As Long

    For json_Index = 1 To VBA.Len(json_Text)
        json_Char = VBA.Mid$(json_Text, json_Index, 1)
        json_AscCode = VBA.AscW(json_Char)

        ' When AscW returns a negative number, it returns the twos complement form of that number.
        ' To convert the twos complement notation into normal binary notation, add 0xFFF to the return result.
        ' https://support.microsoft.com/en-us/kb/272138
        If json_AscCode < 0 Then
            json_AscCode = json_AscCode + 65536
        End If

        ' From spec, ", \, and control characters must be escaped (solidus is optional)

        Select Case json_AscCode
        Case 34
            ' " -> 34 -> \"
            json_Char = "\"""
        Case 92
            ' \ -> 92 -> \\
            json_Char = "\\"
        Case 47
            ' / -> 47 -> \/ (optional)
            If JsonOptions.EscapeSolidus Then
                json_Char = "\/"
            End If
        Case 8
            ' backspace -> 8 -> \b
            json_Char = "\b"
        Case 12
            ' form feed -> 12 -> \f
            json_Char = "\f"
        Case 10
            ' line feed -> 10 -> \n
            json_Char = "\n"
        Case 13
            ' carriage return -> 13 -> \r
            json_Char = "\r"
        Case 9
            ' tab -> 9 -> \t
            json_Char = "\t"
        Case 0 To 31, 127 To 65535
            ' Non-ascii characters -> convert to 4-digit hex
            json_Char = "\u" & VBA.Right$("0000" & VBA.Hex$(json_AscCode), 4)
        End Select

        json_BufferAppend json_Buffer, json_Char, json_BufferPosition, json_BufferLength
    Next json_Index

    json_Encode = json_BufferToString(json_Buffer, json_BufferPosition)
End Function

Private Function json_Peek(json_String As String, ByVal json_Index As Long, Optional json_NumberOfCharacters As Long = 1) As String
    ' "Peek" at the next number of characters without incrementing json_Index (ByVal instead of ByRef)
    json_SkipSpaces json_String, json_Index
    json_Peek = VBA.Mid$(json_String, json_Index, json_NumberOfCharacters)
End Function

Private Sub json_SkipSpaces(json_String As String, ByRef json_Index As Long)
    ' Increment index to skip over spaces
    Do While json_Index > 0 And json_Index <= VBA.Len(json_String) And VBA.Mid$(json_String, json_Index, 1) = " "
        json_Index = json_Index + 1
    Loop
End Sub

Private Function json_StringIsLargeNumber(json_String As Variant) As Boolean
    ' Check if the given string is considered a "large number"
    ' (See json_ParseNumber)

    Dim json_Length As Long
    Dim json_CharIndex As Long
    json_Length = VBA.Len(json_String)

    ' Length with be at least 16 characters and assume will be less than 100 characters
    If json_Length >= 16 And json_Length <= 100 Then
        Dim json_CharCode As String

        json_StringIsLargeNumber = True

        For json_CharIndex = 1 To json_Length
            json_CharCode = VBA.Asc(VBA.Mid$(json_String, json_CharIndex, 1))
            Select Case json_CharCode
            ' Look for .|0-9|E|e
            Case 46, 48 To 57, 69, 101
                ' Continue through characters
            Case Else
                json_StringIsLargeNumber = False
                Exit Function
            End Select
        Next json_CharIndex
    End If
End Function

Private Function json_ParseErrorMessage(json_String As String, ByRef json_Index As Long, ErrorMessage As String)
    ' Provide detailed parse error message, including details of where and what occurred
    '
    ' Example:
    ' Error parsing JSON:
    ' {"abcde":True}
    '          ^
    ' Expecting 'STRING', 'NUMBER', null, true, false, '{', or '['

    Dim json_StartIndex As Long
    Dim json_StopIndex As Long

    ' Include 10 characters before and after error (if possible)
    json_StartIndex = json_Index - 10
    json_StopIndex = json_Index + 10
    If json_StartIndex <= 0 Then
        json_StartIndex = 1
    End If
    If json_StopIndex > VBA.Len(json_String) Then
        json_StopIndex = VBA.Len(json_String)
    End If

    json_ParseErrorMessage = "Error parsing JSON:" & VBA.vbNewLine & _
                             VBA.Mid$(json_String, json_StartIndex, json_StopIndex - json_StartIndex + 1) & VBA.vbNewLine & _
                             VBA.Space$(json_Index - json_StartIndex) & "^" & VBA.vbNewLine & _
                             ErrorMessage
End Function

Private Sub json_BufferAppend(ByRef json_Buffer As String, _
                              ByRef json_Append As Variant, _
                              ByRef json_BufferPosition As Long, _
                              ByRef json_BufferLength As Long)
    ' VBA can be slow to append strings due to allocating a new string for each append
    ' Instead of using the traditional append, allocate a large empty string and then copy string at append position
    '
    ' Example:
    ' Buffer: "abc  "
    ' Append: "def"
    ' Buffer Position: 3
    ' Buffer Length: 5
    '
    ' Buffer position + Append length > Buffer length -> Append chunk of blank space to buffer
    ' Buffer: "abc       "
    ' Buffer Length: 10
    '
    ' Put "def" into buffer at position 3 (0-based)
    ' Buffer: "abcdef    "
    '
    ' Approach based on cStringBuilder from vbAccelerator
    ' http://www.vbaccelerator.com/home/VB/Code/Techniques/RunTime_Debug_Tracing/VB6_Tracer_Utility_zip_cStringBuilder_cls.asp
    '
    ' and clsStringAppend from Philip Swannell
    ' https://github.com/VBA-tools/VBA-JSON/pull/82

    Dim json_AppendLength As Long
    Dim json_LengthPlusPosition As Long

    json_AppendLength = VBA.Len(json_Append)
    json_LengthPlusPosition = json_AppendLength + json_BufferPosition

    If json_LengthPlusPosition > json_BufferLength Then
        ' Appending would overflow buffer, add chunk
        ' (double buffer length or append length, whichever is bigger)
        Dim json_AddedLength As Long
        json_AddedLength = IIf(json_AppendLength > json_BufferLength, json_AppendLength, json_BufferLength)

        json_Buffer = json_Buffer & VBA.Space$(json_AddedLength)
        json_BufferLength = json_BufferLength + json_AddedLength
    End If

    ' Note: Namespacing with VBA.Mid$ doesn't work properly here, throwing compile error:
    ' Function call on left-hand side of assignment must return Variant or Object
    Mid$(json_Buffer, json_BufferPosition + 1, json_AppendLength) = CStr(json_Append)
    json_BufferPosition = json_BufferPosition + json_AppendLength
End Sub

Private Function json_BufferToString(ByRef json_Buffer As String, ByVal json_BufferPosition As Long) As String
    If json_BufferPosition > 0 Then
        json_BufferToString = VBA.Left$(json_Buffer, json_BufferPosition)
    End If
End Function

''
' VBA-UTC v1.0.6
' (c) Tim Hall - https://github.com/VBA-tools/VBA-UtcConverter
'
' UTC/ISO 8601 Converter for VBA
'
' Errors:
' 10011 - UTC parsing error
' 10012 - UTC conversion error
' 10013 - ISO 8601 parsing error
' 10014 - ISO 8601 conversion error
'
' @module UtcConverter
' @author tim.hall.engr@gmail.com
' @license MIT (http://www.opensource.org/licenses/mit-license.php)
'' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '

' (Declarations moved to top)

' ============================================= '
' Public Methods
' ============================================= '

''
' Parse UTC date to local date
'
' @method ParseUtc
' @param {Date} UtcDate
' @return {Date} Local date
' @throws 10011 - UTC parsing error
''
Public Function ParseUtc(utc_UtcDate As Date) As Date
    On Error GoTo utc_ErrorHandling

#If Mac Then
    ParseUtc = utc_ConvertDate(utc_UtcDate)
#Else
    Dim utc_TimeZoneInfo As utc_TIME_ZONE_INFORMATION
    Dim utc_LocalDate As utc_SYSTEMTIME

    utc_GetTimeZoneInformation utc_TimeZoneInfo
    utc_SystemTimeToTzSpecificLocalTime utc_TimeZoneInfo, utc_DateToSystemTime(utc_UtcDate), utc_LocalDate

    ParseUtc = utc_SystemTimeToDate(utc_LocalDate)
#End If

    Exit Function

utc_ErrorHandling:
    Err.Raise 10011, "UtcConverter.ParseUtc", "UTC parsing error: " & Err.Number & " - " & Err.Description
End Function

''
' Convert local date to UTC date
'
' @method ConvertToUrc
' @param {Date} utc_LocalDate
' @return {Date} UTC date
' @throws 10012 - UTC conversion error
''
Public Function ConvertToUtc(utc_LocalDate As Date) As Date
    On Error GoTo utc_ErrorHandling

#If Mac Then
    ConvertToUtc = utc_ConvertDate(utc_LocalDate, utc_ConvertToUtc:=True)
#Else
    Dim utc_TimeZoneInfo As utc_TIME_ZONE_INFORMATION
    Dim utc_UtcDate As utc_SYSTEMTIME

    utc_GetTimeZoneInformation utc_TimeZoneInfo
    utc_TzSpecificLocalTimeToSystemTime utc_TimeZoneInfo, utc_DateToSystemTime(utc_LocalDate), utc_UtcDate

    ConvertToUtc = utc_SystemTimeToDate(utc_UtcDate)
#End If

    Exit Function

utc_ErrorHandling:
    Err.Raise 10012, "UtcConverter.ConvertToUtc", "UTC conversion error: " & Err.Number & " - " & Err.Description
End Function

''
' Parse ISO 8601 date string to local date
'
' @method ParseIso
' @param {Date} utc_IsoString
' @return {Date} Local date
' @throws 10013 - ISO 8601 parsing error
''
Public Function ParseIso(utc_IsoString As String) As Date
    On Error GoTo utc_ErrorHandling

    Dim utc_Parts() As String
    Dim utc_DateParts() As String
    Dim utc_TimeParts() As String
    Dim utc_OffsetIndex As Long
    Dim utc_HasOffset As Boolean
    Dim utc_NegativeOffset As Boolean
    Dim utc_OffsetParts() As String
    Dim utc_Offset As Date

    utc_Parts = VBA.Split(utc_IsoString, "T")
    utc_DateParts = VBA.Split(utc_Parts(0), "-")
    ParseIso = VBA.DateSerial(VBA.CInt(utc_DateParts(0)), VBA.CInt(utc_DateParts(1)), VBA.CInt(utc_DateParts(2)))

    If UBound(utc_Parts) > 0 Then
        If VBA.InStr(utc_Parts(1), "Z") Then
            utc_TimeParts = VBA.Split(VBA.Replace(utc_Parts(1), "Z", ""), ":")
        Else
            utc_OffsetIndex = VBA.InStr(1, utc_Parts(1), "+")
            If utc_OffsetIndex = 0 Then
                utc_NegativeOffset = True
                utc_OffsetIndex = VBA.InStr(1, utc_Parts(1), "-")
            End If

            If utc_OffsetIndex > 0 Then
                utc_HasOffset = True
                utc_TimeParts = VBA.Split(VBA.Left$(utc_Parts(1), utc_OffsetIndex - 1), ":")
                utc_OffsetParts = VBA.Split(VBA.Right$(utc_Parts(1), Len(utc_Parts(1)) - utc_OffsetIndex), ":")

                Select Case UBound(utc_OffsetParts)
                Case 0
                    utc_Offset = TimeSerial(VBA.CInt(utc_OffsetParts(0)), 0, 0)
                Case 1
                    utc_Offset = TimeSerial(VBA.CInt(utc_OffsetParts(0)), VBA.CInt(utc_OffsetParts(1)), 0)
                Case 2
                    ' VBA.Val does not use regional settings, use for seconds to avoid decimal/comma issues
                    utc_Offset = TimeSerial(VBA.CInt(utc_OffsetParts(0)), VBA.CInt(utc_OffsetParts(1)), Int(VBA.Val(utc_OffsetParts(2))))
                End Select

                If utc_NegativeOffset Then: utc_Offset = -utc_Offset
            Else
                utc_TimeParts = VBA.Split(utc_Parts(1), ":")
            End If
        End If

        Select Case UBound(utc_TimeParts)
        Case 0
            ParseIso = ParseIso + VBA.TimeSerial(VBA.CInt(utc_TimeParts(0)), 0, 0)
        Case 1
            ParseIso = ParseIso + VBA.TimeSerial(VBA.CInt(utc_TimeParts(0)), VBA.CInt(utc_TimeParts(1)), 0)
        Case 2
            ' VBA.Val does not use regional settings, use for seconds to avoid decimal/comma issues
            ParseIso = ParseIso + VBA.TimeSerial(VBA.CInt(utc_TimeParts(0)), VBA.CInt(utc_TimeParts(1)), Int(VBA.Val(utc_TimeParts(2))))
        End Select

        ParseIso = ParseUtc(ParseIso)

        If utc_HasOffset Then
            ParseIso = ParseIso - utc_Offset
        End If
    End If

    Exit Function

utc_ErrorHandling:
    Err.Raise 10013, "UtcConverter.ParseIso", "ISO 8601 parsing error for " & utc_IsoString & ": " & Err.Number & " - " & Err.Description
End Function

''
' Convert local date to ISO 8601 string
'
' @method ConvertToIso
' @param {Date} utc_LocalDate
' @return {Date} ISO 8601 string
' @throws 10014 - ISO 8601 conversion error
''
Public Function ConvertToIso(utc_LocalDate As Date) As String
    On Error GoTo utc_ErrorHandling

    ConvertToIso = VBA.Format$(ConvertToUtc(utc_LocalDate), "yyyy-mm-ddTHH:mm:ss.000Z")

    Exit Function

utc_ErrorHandling:
    Err.Raise 10014, "UtcConverter.ConvertToIso", "ISO 8601 conversion error: " & Err.Number & " - " & Err.Description
End Function

' ============================================= '
' Private Functions
' ============================================= '

#If Mac Then

Private Function utc_ConvertDate(utc_Value As Date, Optional utc_ConvertToUtc As Boolean = False) As Date
    Dim utc_ShellCommand As String
    Dim utc_Result As utc_ShellResult
    Dim utc_Parts() As String
    Dim utc_DateParts() As String
    Dim utc_TimeParts() As String

    If utc_ConvertToUtc Then
        utc_ShellCommand = "date -ur `date -jf '%Y-%m-%d %H:%M:%S' " & _
            "'" & VBA.Format$(utc_Value, "yyyy-mm-dd HH:mm:ss") & "' " & _
            " +'%s'` +'%Y-%m-%d %H:%M:%S'"
    Else
        utc_ShellCommand = "date -jf '%Y-%m-%d %H:%M:%S %z' " & _
            "'" & VBA.Format$(utc_Value, "yyyy-mm-dd HH:mm:ss") & " +0000' " & _
            "+'%Y-%m-%d %H:%M:%S'"
    End If

    utc_Result = utc_ExecuteInShell(utc_ShellCommand)

    If utc_Result.utc_Output = "" Then
        Err.Raise 10015, "UtcConverter.utc_ConvertDate", "'date' command failed"
    Else
        utc_Parts = Split(utc_Result.utc_Output, " ")
        utc_DateParts = Split(utc_Parts(0), "-")
        utc_TimeParts = Split(utc_Parts(1), ":")

        utc_ConvertDate = DateSerial(utc_DateParts(0), utc_DateParts(1), utc_DateParts(2)) + _
            TimeSerial(utc_TimeParts(0), utc_TimeParts(1), utc_TimeParts(2))
    End If
End Function

Private Function utc_ExecuteInShell(utc_ShellCommand As String) As utc_ShellResult
#If VBA7 Then
    Dim utc_File As LongPtr
    Dim utc_Read As LongPtr
#Else
    Dim utc_File As Long
    Dim utc_Read As Long
#End If

    Dim utc_Chunk As String

    On Error GoTo utc_ErrorHandling
    utc_File = utc_popen(utc_ShellCommand, "r")

    If utc_File = 0 Then: Exit Function

    Do While utc_feof(utc_File) = 0
        utc_Chunk = VBA.Space$(50)
        utc_Read = CLng(utc_fread(utc_Chunk, 1, Len(utc_Chunk) - 1, utc_File))
        If utc_Read > 0 Then
            utc_Chunk = VBA.Left$(utc_Chunk, CLng(utc_Read))
            utc_ExecuteInShell.utc_Output = utc_ExecuteInShell.utc_Output & utc_Chunk
        End If
    Loop

utc_ErrorHandling:
    utc_ExecuteInShell.utc_ExitCode = CLng(utc_pclose(utc_File))
End Function

#Else

Private Function utc_DateToSystemTime(utc_Value As Date) As utc_SYSTEMTIME
    utc_DateToSystemTime.utc_wYear = VBA.Year(utc_Value)
    utc_DateToSystemTime.utc_wMonth = VBA.Month(utc_Value)
    utc_DateToSystemTime.utc_wDay = VBA.Day(utc_Value)
    utc_DateToSystemTime.utc_wHour = VBA.Hour(utc_Value)
    utc_DateToSystemTime.utc_wMinute = VBA.Minute(utc_Value)
    utc_DateToSystemTime.utc_wSecond = VBA.Second(utc_Value)
    utc_DateToSystemTime.utc_wMilliseconds = 0
End Function

Private Function utc_SystemTimeToDate(utc_Value As utc_SYSTEMTIME) As Date
    utc_SystemTimeToDate = DateSerial(utc_Value.utc_wYear, utc_Value.utc_wMonth, utc_Value.utc_wDay) + _
        TimeSerial(utc_Value.utc_wHour, utc_Value.utc_wMinute, utc_Value.utc_wSecond)
End Function

#End If
