Program ShowEpoch;

uses
  SysUtils;

begin
  Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', 0));
end.
