Program Combinations;

const
 m_max = 3;
 n_max = 5;
var
 combination: array [1..m_max] of integer;

 procedure generate(m: integer);
  var
   n, i: integer;
  begin
   if (m > m_max) then
    begin
    for i := 1 to m_max do
     write (combination[i], ' ');
    writeln;
    end
   else
    for n := 1 to n_max do
     if ((m = 1) or (n > combination[m-1])) then
      begin
       combination[m] := n;
       generate(m + 1);
      end;
   end;

begin
 generate(1);
end.
