Program PythagoreanTriples (output);

var
  total, prim, maxPeri: int64;

procedure newTri(s0, s1, s2: int64);
  var
    p: int64;
  begin
    p := s0 + s1 + s2;
    if p <= maxPeri then
    begin
      inc(prim);
      total := total + maxPeri div p;
      newTri( s0 + 2*(-s1+s2),  2*( s0+s2) - s1,  2*( s0-s1+s2) + s2);
      newTri( s0 + 2*( s1+s2),  2*( s0+s2) + s1,  2*( s0+s1+s2) + s2);
      newTri(-s0 + 2*( s1+s2),  2*(-s0+s2) + s1,  2*(-s0+s1+s2) + s2);
    end;
  end;

begin
  maxPeri := 100;
  while maxPeri <= 1e10 do
  begin
    prim := 0;
    total := 0;
    newTri(3, 4, 5);
    writeln('Up to ', maxPeri, ': ', total, ' triples, ', prim, ' primitives.');
    maxPeri := maxPeri * 10;
  end;
end.
