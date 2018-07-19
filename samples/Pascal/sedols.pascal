program Sedols(output);

function index(c: char): integer;
  const
    alpha = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  var
    i: integer;
  begin
    index := 0;
    for i := low(alpha) to high(alpha) do
      if c = alpha[i] then
        index := i;
  end;

function checkdigit(c: string): char;
  const
    weight: array [1..6] of integer = (1, 3, 1, 7, 3, 9);
  var
    i, sum: integer;
  begin
    sum := 0;
    for i := 1 to 6 do
      sum := sum + (index(c[i]) - 1) * weight[i];
    checkdigit := char((10 - (sum mod 10)) mod 10 + 48);
  end;

const
  codes: array [1..11] of string =
    ('710889', 'B0YBKJ', '406566', 'B0YBLH',
     '228276', 'B0YBKL', '557910', 'B0YBKR',
     '585284', 'B0YBKT', 'B00030');

var
  seforl: string;
  i: integer;

begin
  for i := low(codes) to high(codes) do
  begin
    seforl := codes[i];
    setlength(seforl, 7);
    seforl[7] := checkdigit(codes[i]);
    writeln(codes[i], ' -> ', seforl);
  end;
end.
