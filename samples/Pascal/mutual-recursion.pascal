Program MutualRecursion;

{M definition comes after F which uses it}
function M(n : Integer) : Integer; forward;

function F(n : Integer) : Integer;
begin
   if n = 0 then
      F := 1
   else
      F := n - M(F(n-1));
end;

function M(n : Integer) : Integer;
begin
   if n = 0 then
      M := 0
   else
      M := n - F(M(n-1));
end;

var
   i : Integer;

begin
   for i := 0 to 19 do begin
      write(F(i) : 4)
   end;
   writeln;
   for i := 0 to 19 do begin
      write(M(i) : 4)
   end;
   writeln;
end.
