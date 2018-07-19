Program HostName;

uses
  unix;

begin
  writeln('The name of this computer is: ', GetHostName);
end.
