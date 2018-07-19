Program VectorProduct (output);

type
  Tvector = record
    x, y, z: double
  end;

function dotProduct(a, b: Tvector): double;
begin
  dotProduct := a.x*b.x + a.y*b.y + a.z*b.z;
end;

function crossProduct(a, b: Tvector): Tvector;
begin
  crossProduct.x := a.y*b.z - a.z*b.y;
  crossProduct.y := a.z*b.x - a.x*b.z;
  crossProduct.z := a.x*b.y - a.y*b.x;
end;

function scalarTripleProduct(a, b, c: Tvector): double;
begin
  scalarTripleProduct := dotProduct(a, crossProduct(b, c));
end;

function vectorTripleProduct(a, b, c: Tvector): Tvector;
begin
  vectorTripleProduct := crossProduct(a, crossProduct(b, c));
end;

procedure printVector(a: Tvector);
begin
  writeln(a.x:15:8, a.y:15:8, a.z:15:8);
end;

var
  a: Tvector = (x: 3; y:  4; z:  5);
  b: Tvector = (x: 4; y:  3; z:  5);
  c: Tvector = (x:-5; y:-12; z:-13);

begin
  write('a: '); printVector(a);
  write('b: '); printVector(b);
  write('c: '); printVector(c);
  writeln('a . b: ', dotProduct(a,b):15:8);
  write('a x b: '); printVector(crossProduct(a,b));
  writeln('a . (b x c): ', scalarTripleProduct(a,b,c):15:8);
  write('a x (b x c): '); printVector(vectorTripleProduct(a,b,c));
end.
