uses
  uw27294;

var
  p : procedure;

procedure test;

begin
  p:=@test;
  writeln('OK');
end;

procedure global;
begin
  p:=nil;
  test;
  p();
end;

begin
  global;
  uw27294.global;
end.


