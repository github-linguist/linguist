program perm;

var
	p: array[1 .. 12] of integer;
	is_last: boolean;
	n: integer;

procedure next;
var i, j, k, t: integer;
begin
is_last := true;
i := n - 1;
while i > 0 do
	begin
	if p[i] < p[i + 1] then
		begin
		is_last := false;
		break;
		end;
	i := i - 1;
	end;

if not is_last then
	begin
	j := i + 1;
	k := n;
	while j < k do
		begin
		t := p[j];
		p[j] := p[k];
		p[k] := t;
		j := j + 1;
		k := k - 1;
		end;
		
	j := n;
	while p[j] > p[i] do j := j - 1;
	j := j + 1;

	t := p[i];
	p[i] := p[j];
	p[j] := t;
	end;
end;

procedure print;
var i: integer;
begin
for i := 1 to n do write(p[i], ' ');
writeln;
end;

procedure init;
var i: integer;
begin
n := 0;
while (n < 1) or (n > 10) do
	begin
	write('Enter n (1 <= n <= 10): ');
	readln(n);
	end;
for i := 1 to n do p[i] := i;
end;

begin
init;
repeat
	print;
	next;
until is_last;
end.
