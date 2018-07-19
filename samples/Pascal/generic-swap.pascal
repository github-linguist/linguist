program generictest;

{$mode objfpc}

type
  generic TSwap<T> = procedure (var a, b: T);

procedure Proc1(var a, b: integer);
  var
    temp: integer;
  begin
    temp := a;
    a := b;
    b := temp;
  end;

var
  S, T: integer;
  SwapInt: specialize TSwap<integer>;

begin
  S := 4;
  T := 3;
  SwapInt := @Proc1;
  writeln(S, T:2);
  SwapInt(S, T);
  writeln(S, T:2);
end.
