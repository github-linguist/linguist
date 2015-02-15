program RemLines;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils;

type
  TRLResponse=(rlrOk, rlrEmptyFile, rlrNotEnoughLines);

function RemoveLines(const FileName: String; const From, Count: Integer): TRLResponse;
const
  LineOffs = Length(LineEnding);

var
  TIn, TOut: TFileStream;
  tmpFn, MemBuff, FileBuff: String;
  EndingPos, CharRead, LineNumber: Integer;

  procedure WriteLine(Line: String);
  begin
    if ((From > 1) and (LineNumber = 1)) or ((From = 1) and (LineNumber = (From+Count))) then
      // First line to write, without LineEnding => Line unchanged
    else if ((From = 1) or (From <= LineNumber)) and (LineNumber < (From+Count)) then
      // No line to write
      Line := ''
    else
      // all other cases, write Line preceded (!) by LineEnding
      Line := LineEnding + Line;
    // Write
    if Line <> '' then
      TOut.Write(Line[1], Length(Line));
  End;

begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('No such file %s', [FileName]);
  if From < 1 then
    raise Exception.Create('First line must be >= 1');

  tmpFn := GetTempFileName(ExtractFilePath(FileName), '');

  TIn := TFileStream.Create(FileName, fmOpenRead);
  try
    TOut := TFileStream.Create(tmpFn, fmCreate);
    try
      FileBuff := StringOfChar(' ', 1024); // Reserve memory in a string
      LineNumber := 0;
      MemBuff := '';
      while True do
      begin
        CharRead := TIn.Read(FileBuff[1], 1024);
        if (CharRead = 0) then
          break; // no more char to process
        MemBuff += Copy(FileBuff, 1, CharRead); // op += is FPC specific
        while True do
        begin
          // LineEnding can contain 1 or 2 chars, depending on the OS
          EndingPos := Pos(LineEnding, MemBuff);
          if EndingPos = 0 then
            break; // EndingLine in the next reading, maybe
          Inc(LineNumber);
          WriteLine(Copy(MemBuff, 1, EndingPos - 1));
          MemBuff := Copy(MemBuff, EndingPos + LineOffs, MaxInt);
          // Loop for another line in MemBuff
        end;
      end;
      Inc(LineNumber);
      WriteLine(MemBuff); // Writes what remains
    finally
      TOut.Free;
    end;
  finally
    TIn.Free;
  end;
  // Temp File replaces the original file.
  if DeleteFile(FileName) then
    RenameFile(tmpFn, FileName)
  else
    raise Exception.Create('Unable to process the file');
  // Response
  if (LineNumber = 0) then
    Result := rlrEmptyFile
  else if (LineNumber < (From+Count-1)) then
    Result := rlrNotEnoughLines
  else
    Result := rlrOk;
End;

var
  FileName: String;

begin
  FileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'test.txt';
  try
    case RemoveLines(FileName, 4, 3) of
    rlrOk: WriteLn('Lines deleted');
    rlrEmptyFile: WriteLn(Format('File "%s" is empty!', [FileName]));
    rlrNotEnoughLines: WriteLn('Can''t delete lines past the end of file');
    end
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;
  ReadLn;
End.
