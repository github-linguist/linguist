Program LeastCommonMultiple(output);

function lcm(a, b: longint): longint;
  begin
    lcm := a;
    while (lcm mod b) <> 0 do
      inc(lcm, a);
  end;

begin
  writeln('The least common multiple of 12 and 18 is: ', lcm(12, 18));
end.
