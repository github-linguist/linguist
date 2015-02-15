{$H+}

program Walk;

uses SysUtils;

var Res: TSearchRec;
    Pattern, Path, Name: String;
    FileAttr: LongInt;
    Attr: Integer;

begin
   Write('File pattern: ');
   ReadLn(Pattern);            { For example .\*.pas }

   Attr := faAnyFile;
   if FindFirst(Pattern, Attr, Res) = 0 then
   begin
      Path := ExtractFileDir(Pattern);
      repeat
         Name := ConcatPaths([Path, Res.Name]);
         FileAttr := FileGetAttr(Name);
         if FileAttr and faDirectory = 0 then
         begin
            { Do something with file name }
            WriteLn(Name);
         end
      until FindNext(Res) <> 0;
   end;
   FindClose(Res);
end.
