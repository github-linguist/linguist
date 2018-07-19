Program ExecuteSystemCommand;

uses
  SysUtils;
begin
  ExecuteProcess('/bin/ls', '-alh');
end.
