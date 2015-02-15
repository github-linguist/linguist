program bogosort;

const
  max = 5;
type
  list = array [1..max] of integer;

{ Print a list }
procedure printa(a: list);
var
  i: integer;
begin
  for i := 1 to max do
    write(a[i], ' ');
  writeln
end;

{ Knuth shuffle }
procedure shuffle(var a: list);
var
  i,k,tmp: integer;
begin
  for i := max downto 2 do begin
     k := random(i) + 1;
     if (a[i] <> a[k]) then begin
       tmp := a[i]; a[i] := a[k]; a[k] := tmp
     end
  end
end;

{ Check for sorted list }
function sorted(a: list): boolean;
var
  i: integer;
begin
  sorted := True;
  for i := 2 to max do
    if (a[i - 1] > a[i]) then begin
      sorted := False; exit
    end
end;

{ Bogosort }
procedure bogo(var a: list);
var
  i: integer;
begin
  i := 1; randomize;
  write(i,': '); printa(a);
  while not sorted(a) do begin
    shuffle(a);
    i := i + 1; write(i,': '); printa(a)
  end
end;

{ Test and display }
var
  a: list;
  i: integer;

begin
  for i := 1 to max do
    a[i] := (max + 1) - i;
  bogo(a);
end.
