Structure fileDataBits
  bitsToWrite.i
  bitsToRead.i
  outputByte.i
  inputByte.i
EndStructure
#BitsPerByte = SizeOf(Byte) * 8
#BitsPerInteger = SizeOf(Integer) * 8

Global Dim fileBitMask(8)
Define i, x
For i = 0 To 8
  fileBitMask(i) = x
  x = (x << 1) + 1
Next

Global NewMap fileDataBits.fileDataBits()
Procedure flushBits(fileID)
  If FindMapElement(fileDataBits(), Str(fileID))
    If fileDataBits()\bitsToWrite > 0
      WriteByte(fileID, fileDataBits()\outputByte << (#BitsPerByte - fileDataBits()\bitsToWrite))
      fileDataBits()\bitsToWrite = 0
      fileDataBits()\outputByte = 0
    EndIf
    DeleteMapElement(fileDataBits())
  EndIf
EndProcedure

Procedure writeBits(fileID, Value.i, bitCount)
  Protected *fileData.fileDataBits = FindMapElement(fileDataBits(), Str(fileID))
  If Not *fileData
    *fileData = AddMapElement(fileDataBits(), Str(fileID))
    If Not *fileData: End: EndIf ;simple error check for lack of memory
  EndIf

  Value << (#BitsPerInteger - bitCount) ;shift value so it's first bit (HSB) is in the highest position
  While bitCount > 0
    If bitCount > #BitsPerByte - *fileData\bitsToWrite
      bitGroupSize = #BitsPerByte - *fileData\bitsToWrite
    Else
      bitGroupSize = bitCount
    EndIf

    *fileData\outputByte << bitGroupSize
    *fileData\outputByte + (Value >> (#BitsPerInteger - bitGroupSize)) & fileBitMask(bitGroupSize)
    Value << bitGroupSize
    *fileData\bitsToWrite + bitGroupSize
    If *fileData\bitsToWrite = #BitsPerByte
      WriteByte(fileID, *fileData\outputByte)
      *fileData\bitsToWrite = 0
      *fileData\outputByte = 0
    EndIf
    bitCount - bitGroupSize
  Wend
EndProcedure

Procedure.i readBits(fileID, bitCount)
  Protected *fileData.fileDataBits = FindMapElement(fileDataBits(), Str(fileID))
  If Not *fileData
    *fileData = AddMapElement(fileDataBits(), Str(fileID))
    If Not *fileData: End: EndIf ;simple error check for lack of memory
  EndIf

  Protected Value.i, bitGroupSize
  While bitCount > 0
    If *fileData\bitsToRead = 0
      If Not Eof(fileID)
        *fileData\inputByte = ReadByte(fileID)
        *fileData\bitsToRead = #BitsPerByte
      Else
        flushBits(fileID)
        Break ;simple error check aborts if nothing left to read
      EndIf
    EndIf

    If bitCount > *fileData\bitsToRead
      bitGroupSize = *fileData\bitsToRead
    Else
      bitGroupSize = bitCount
    EndIf
    Value << bitGroupSize
    Value + (*fileData\inputByte >> (#BitsPerByte - bitGroupSize)) & fileBitMask(bitGroupSize) ;shift last bit being read in byte to the lowest position
    *fileData\inputByte << bitGroupSize
    *fileData\bitsToRead - bitGroupSize
    bitCount - bitGroupSize
  Wend
  ProcedureReturn Value
EndProcedure

;test
Define testWriteString.s, testReadString.s, fileNum, result.s
testWriteString = "This is an ascii string that will be crunched, written, read and expanded."
fileNum = CreateFile(#PB_Any, "BitIO_Test.dat")
If fileNum
  For i = 1 To Len(testWriteString)
    writeBits(fileNum, Asc(Mid(testWriteString, i, 1)), 7)
  Next
  flushBits(fileNum)
  CloseFile(fileNum)
EndIf

fileNum = ReadFile(#PB_Any, "BitIO_Test.dat")
If fileNum
  For i = 1 To Len(testWriteString)
    testReadString + Chr(readBits(fileNum, 7))
  Next
  flushBits(fileNum)
  CloseFile(fileNum)
EndIf

result = "Original ascii string is " + Str(Len(testWriteString)) + " bytes." + #LF$
result + "Filesize written is " + Str(FileSize("test.xxx")) + " bytes." + #LF$
If testReadString = testWriteString
  result + "The expanded string is the same as the original." + #LF$
Else
  result + "The expanded string is not the same as the original." + #LF$
EndIf
result + "Expanded string = '" + testReadString + "'"

MessageRequester("Results", result)
