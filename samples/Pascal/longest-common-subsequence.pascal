Program LongestCommonSubsequence(output);

function lcs(a, b: string): string;
  var
    x, y: string;
    lenga, lengb: integer;
  begin
    lenga := length(a);
    lengb := length(b);
    lcs := '';
    if (lenga >  0) and (lengb >  0) then
      if a[lenga] =  b[lengb] then
        lcs := lcs(copy(a, 1, lenga-1), copy(b, 1, lengb-1)) + a[lenga]
      else
      begin
        x := lcs(a, copy(b, 1, lengb-1));
        y := lcs(copy(a, 1, lenga-1), b);
        if length(x) > length(y) then
          lcs := x
        else
          lcs := y;
      end;
  end;

var
  s1, s2: string;
begin
  s1 := 'thisisatest';
  s2 := 'testing123testing';
  writeln (lcs(s1, s2));
  s1 := '1234';
  s2 := '1224533324';
  writeln (lcs(s1, s2));
end.
