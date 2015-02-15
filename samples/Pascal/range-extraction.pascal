program RangeExtraction;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils;

function RangeExtraction(const Seq: array of integer): String;
const
  SubSeqLen = 3; // minimal length of the range, can be changed.
var
  i, j: Integer;
  Separator: string = '';
begin
  Result := '';
  i := Low(Seq);
  while i <= High(Seq) do
  begin
    j := i;
    // All subsequent values, starting from i, up to High(Seq) possibly
    while ((j < High(Seq)) and ((Seq[j+1]-Seq[j]) = 1)) do
      Inc(j);
    // is it a range ?
    if ((j-i) >= (SubSeqLen-1)) then
    begin
      Result := Result + Format(Separator+'%d-%d',[Seq[i],Seq[j]]);
      i := j+1; // Next value to be processed
      Separator := ',';
    end
    else
    begin
      // Loop, to process the case SubSeqLen > 3
      while i<=j do
      begin
        Result := Result + Format(Separator+'%d',[Seq[i]]);
        Inc(i); // Next value to be processed
        Separator := ',';
      end;
    end;
  end;
End;

procedure DisplayRange(const Seq: array of integer);
var
  i: Integer;
begin
  Write(Format('[%d', [Seq[Low(Seq)]]));
  for i := Low(Seq) + 1 to High(Seq) do
    Write(Format(',%d', [Seq[i]]));
  WriteLn('] => ' + RangeExtraction(Seq));
  WriteLn;
End;

begin
  DisplayRange([0]);
  DisplayRange([0,1]);
  DisplayRange([0,2]);
  DisplayRange([0,1,2]);
  DisplayRange([0,1,2,3]);
  DisplayRange([0,1,2,3,4,5,6,7]);
  DisplayRange([0,2,3,4,5,6,7,9]);
  DisplayRange([0,2,4,6,8,10]);
  DisplayRange([0,1,2,3,4,5,6,7,9]);
  DisplayRange([0,1,2,3,4,6,9,10,11,12]);

  DisplayRange([
      0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
     15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
     25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
     37, 38, 39]);
  ReadLn;
end.
