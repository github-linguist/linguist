Program Example45;

{ Program to demonstrate the SumOfSquares function. }

Uses math;

Var
  I : 1..100;
  ExArray : Array[1..100] of Float;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max             : ',MaxValue(ExArray):8:4);
  Writeln('Min             : ',MinValue(ExArray):8:4);
  Writeln('Sum squares     : ',SumOfSquares(ExArray):8:4);
  Writeln('Sum squares (b) : ',SumOfSquares(@ExArray[1],100):8:4);
end.
