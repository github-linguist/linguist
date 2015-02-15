program Knuth;

const
  max = 10;
type
  list = array [1..max] of integer;

procedure shuffle(var a: list);
var
  i,k,tmp: integer;
begin
  randomize;
  for i := max downto 2 do begin
     k := random(i) + 1;
     if (a[i] <> a[k]) then begin
       tmp := a[i]; a[i] := a[k]; a[k] := tmp
     end
  end
end;

{ Test and display }
var
 a: list;
 i: integer;

begin
  for i := 1 to max do
    a[i] := i;
  shuffle(a);
  for i := 1 to max do
    write(a[i], ' ');
  writeln
end.
